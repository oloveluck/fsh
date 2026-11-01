#lang racket/base

;;; cli.rkt — `raco fsh`: the command line for the FHIR Shorthand package.
;;;
;;;   raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0]
;;;       compile one file, or every .fsh in a directory (SUSHI-style whole-project compile), to FHIR
;;;       JSON files named <resourceType>-<id>.json in <outdir> (default: ./fsh-generated next to the
;;;       input). --snapshot also emits StructureDefinition snapshots (needs the fhir-spec package).
;;;       --fhir-version picks the target (default 5.0.0): 4.0.1 compiles an R4 IG against the R4 tower.
;;;   raco fsh --decompile <file.json>...   the reverse direction (GoFSH-style): print FSH text
;;; Exit codes: 0 ok · 1 FSH error · 2 usage.

(require json
         (only-in racket/file file->string make-directory*)
         (only-in racket/path path-only)
         (only-in fhir/sd/sd-index sd-index-available? current-sd-packages)
         (only-in fhir/spec-package spec-package-path)
         (only-in fsh/compile fsh->fhir emit-snapshot? canonical-base fsh-fhir-version)
         (only-in fsh/decompile fhir->fsh))

(provide compile-path decompile-files)

;; fsh-files : path -> (listof path)   the .fsh files of a file-or-directory argument, sorted
(define (fsh-files src)
  (cond [(directory-exists? src)
         ;; recursive, like SUSHI's input/fsh (IGs organise sources in subfolders)
         (sort (for/list ([f (in-directory src)]
                          #:when (regexp-match? #rx"[.]fsh$" (path->string f)))
                 f)
               string<? #:key path->string)]
        [(file-exists? src) (list (if (path? src) src (string->path src)))]
        [else (eprintf "fsh: no such file or directory: ~a\n" src) (exit 2)]))

;; compile-path : path-string (or/c path-string #f) boolean (or/c string #f) -> void
(define (compile-path src out snapshot? canonical [fhir-version "5.0.0"])
  ;; one tower per process: register the one this compile targets
  (define r4? (regexp-match? #rx"^4[.]" fhir-version))
  (dynamic-require (if r4? 'fhir/r4/schemas 'fhir/r5/schemas) #f)
  (define files (fsh-files src))
  (define text
    (apply string-append
           (for/list ([f (in-list files)])
             (string-append (regexp-replace #px"^#lang[^\n]*\n" (file->string f) "") "\n"))))
  (when (and snapshot? (not (sd-index-available?)))
    (eprintf "fsh: --snapshot needs the R5 core package: raco pkg install fhir-spec\n") (exit 1))
  (define artifacts
    (parameterize ([emit-snapshot? snapshot?]
                   [canonical-base (or canonical (canonical-base))]
                   [fsh-fhir-version fhir-version]
                   [current-sd-packages
                    (let ([core (spec-package-path (if r4? "hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz") #:optional? #t)])
                      (and core (list core)))])
      (fsh->fhir text)))
  (define outdir
    (or out (build-path (if (directory-exists? src) src (or (path-only (car files)) (current-directory)))
                        "fsh-generated")))
  (make-directory* outdir)
  (for ([a (in-list artifacts)])
    (define name (format "~a-~a.json" (hash-ref a 'resourceType "Resource") (hash-ref a 'id "instance")))
    (call-with-output-file (build-path outdir name) #:exists 'replace
      (lambda (o) (write-string (jsexpr->string a) o)))
    (printf "  ~a\n" name))
  (printf "fsh: wrote ~a artifact(s) to ~a\n" (length artifacts) outdir))

;; decompile-files : (listof path-string) -> void   print one FSH document for the given FHIR JSON files
(define (decompile-files files)
  (dynamic-require 'fhir/r5/schemas #f)                 ; the decompiler reads element types from the tower
  (define jss (for/list ([f (in-list files)]) (call-with-input-file f read-json)))
  (displayln (apply fhir->fsh jss)))

(define (usage)
  (eprintf "usage: raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0]\n       raco fsh --decompile <file.json>...\n")
  (exit 2))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
    [(null? args) (usage)]
    [(equal? (car args) "--decompile") (if (pair? (cdr args)) (decompile-files (cdr args)) (usage))]
    [else
     (let loop ([as args] [src #f] [out #f] [snapshot? #f] [canonical #f] [fv "5.0.0"])
       (cond
         [(null? as) (if src (compile-path src out snapshot? canonical fv) (usage))]
         [(and (equal? (car as) "-o") (pair? (cdr as))) (loop (cddr as) src (cadr as) snapshot? canonical fv)]
         [(equal? (car as) "--snapshot") (loop (cdr as) src out #t canonical fv)]
         [(and (equal? (car as) "--canonical") (pair? (cdr as))) (loop (cddr as) src out snapshot? (cadr as) fv)]
         [(and (equal? (car as) "--fhir-version") (pair? (cdr as))) (loop (cddr as) src out snapshot? canonical (cadr as))]
         [(regexp-match? #rx"^-" (car as)) (usage)]
         [src (usage)]
         [else (loop (cdr as) (car as) out snapshot? canonical fv)]))]))
