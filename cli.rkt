#lang racket/base

;;; cli.rkt — `raco fsh`: the command line for the FHIR Shorthand package.
;;;
;;;   raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0]
;;;       compile one file, or every .fsh in a directory (SUSHI-style whole-project compile), to FHIR
;;;       JSON files named <resourceType>-<id>.json in <outdir> (default: ./fsh-generated next to the
;;;       input). --snapshot also emits StructureDefinition snapshots (needs the fhir-spec package).
;;;       Errors: file:line:col diagnostics on stderr (exit 1); --format json|outcome prints them on stdout
;;;       as {"diagnostics":[…]} or a FHIR OperationOutcome instead (fhir/diagnostic).
;;;       --fhir-version picks the target (default 5.0.0): 4.0.1 compiles an R4 IG against the R4 tower.
;;;       A sushi-config.yaml beside the sources (or one/two levels up) supplies canonical, fhirVersion
;;;       and dependencies (packages located in fhir-spec as <id>-<version>.tgz); flags override it.
;;;   raco fsh --decompile <file.json>...   the reverse direction (GoFSH-style): print FSH text
;;; Exit codes: 0 ok · 1 FSH error · 2 usage.

(require json (only-in racket/list append*)
         (only-in racket/file file->string make-directory*)
         (only-in racket/path path-only)
         (only-in fhir/sd/sd-index sd-index-available? current-sd-packages)
         (only-in fhir/spec-package spec-package-path)
         (only-in fsh/compile compile-doc/errors entity-label emit-snapshot? canonical-base fsh-fhir-version)
         (only-in fsh/parser/parse parse-fsh-doc/located)
         (only-in fhir/diagnostic diagnostic emit-diagnostics format-flag exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in fsh/ig-config read-ig-config ig-config-canonical ig-config-fhir-version ig-config-dependencies
                  dependency-packages)
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

;; find-config : path -> path|#f   the project's sushi-config.yaml: next to the sources or one/two
;; levels up (SUSHI projects keep FSH in input/fsh/)
(define (find-config src)
  (define dir (simplify-path (if (directory-exists? src) src (or (path-only src) (current-directory)))))
  (for/or ([d (list dir (build-path dir 'up) (build-path dir 'up 'up))])
    (define f (simplify-path (build-path d "sushi-config.yaml")))
    (and (file-exists? f) f)))

;; compile-path : path-string (or/c path-string #f) boolean (or/c string #f) (or/c string #f) -> void
;; a sushi-config.yaml supplies the canonical, the FHIR version and the DEPENDENCY packages (their
;; profiles/extensions/value sets resolve Parent and InstanceOf) unless the flags say otherwise
(define (compile-path src out snapshot? canonical* [fhir-version* #f] #:format [fmt 'text])
  (define cfg (let ([f (find-config src)]) (and f (read-ig-config f))))
  (define canonical (or canonical* (and cfg (ig-config-canonical cfg))))
  (define fhir-version (or fhir-version* (and cfg (ig-config-fhir-version cfg)) "5.0.0"))
  (define-values (dep-packages missing) (dependency-packages (if cfg (ig-config-dependencies cfg) '())))
  (for ([d (in-list missing)])
    (eprintf "fsh: dependency ~a ~a not found (looked for ~a-~a.tgz in fhir-spec); its profiles will not resolve\n"
             (car d) (cdr d) (car d) (cdr d)))
  ;; one tower per process: register the one this compile targets
  (define r4? (regexp-match? #rx"^4[.]" fhir-version))
  (dynamic-require (if r4? 'fhir/r4/schemas 'fhir/r5/schemas) #f)
  (define files (fsh-files src))
  ;; each file parses on its own, so positions name the right file; a leading #lang line is blanked
  ;; (not removed) so line numbers stay true
  (define texts (for/hash ([f (in-list files)])
                  (values (path->string f) (regexp-replace #px"^#lang[^\n]*" (file->string f) ""))))
  (define parse-errors '())
  (define located
    (append*
     (for/list ([f (in-list files)])
       (with-handlers ([exn:fail:diagnostic? (lambda (e) (set! parse-errors (cons (exn:fail:diagnostic-diagnostic e) parse-errors)) '())])
         (parse-fsh-doc/located (hash-ref texts (path->string f)) #:source (path->string f))))))
  (define (report ds) (emit-diagnostics ds #:format fmt #:text-of (lambda (s) (and s (hash-ref texts s #f)))))
  (when (pair? parse-errors) (report (reverse parse-errors)) (exit 1))
  (when (and snapshot? (not (sd-index-available?)))
    (eprintf "fsh: --snapshot needs the R5 core package: raco pkg install fhir-spec\n") (exit 1))
  (define artifacts
    (parameterize ([emit-snapshot? snapshot?]
                   [canonical-base (or canonical (canonical-base))]
                   [fsh-fhir-version fhir-version]
                   [current-sd-packages
                    (let ([core (spec-package-path (if r4? "hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz") #:optional? #t)])
                      (append dep-packages (if core (list core) '())))])
      (let-values ([(out errors) (compile-doc/errors (map car located))])
        (unless (null? errors)
          ;; an entity that fails to compile: positioned at its declaration
          (define at (for/hash ([p (in-list located)]) (values (entity-label (car p)) (cdr p))))
          (report (for/list ([e (in-list (reverse errors))])
                    (diagnostic 'fsh 'compile 'error (format "~a: ~a" (car e) (exn-message (cdr e)))
                                (hash-ref at (car e) #f))))
          (exit 1))
        out)))
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
  (eprintf "usage: raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0] [--format text|json|outcome]\n       raco fsh --decompile <file.json>...\n")
  (exit 2))

(module+ main
  (define-values (fmt args) (format-flag (vector->list (current-command-line-arguments))))
  (cond
    [(null? args) (usage)]
    [(equal? (car args) "--decompile") (if (pair? (cdr args)) (decompile-files (cdr args)) (usage))]
    [else
     (let loop ([as args] [src #f] [out #f] [snapshot? #f] [canonical #f] [fv #f])
       (cond
         [(null? as) (if src (compile-path src out snapshot? canonical fv #:format fmt) (usage))]
         [(and (equal? (car as) "-o") (pair? (cdr as))) (loop (cddr as) src (cadr as) snapshot? canonical fv)]
         [(equal? (car as) "--snapshot") (loop (cdr as) src out #t canonical fv)]
         [(and (equal? (car as) "--canonical") (pair? (cdr as))) (loop (cddr as) src out snapshot? (cadr as) fv)]
         [(and (equal? (car as) "--fhir-version") (pair? (cdr as))) (loop (cddr as) src out snapshot? canonical (cadr as))]
         [(regexp-match? #rx"^-" (car as)) (usage)]
         [src (usage)]
         [else (loop (cdr as) (car as) out snapshot? canonical fv)]))]))
