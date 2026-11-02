#lang racket/base

;;; cli.rkt — `raco fsh`: the command line for the FHIR Shorthand package.
;;;
;;;   raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0]
;;;       compile one file, or every .fsh in a directory (SUSHI-style whole-project compile), to FHIR
;;;       JSON files named <resourceType>-<id>.json in <outdir> (default: ./fsh-generated next to the
;;;       input). --snapshot also emits StructureDefinition snapshots (needs the fhir-spec package).
;;;       Errors: file:line:col diagnostics on stderr (exit 1); --format json|outcome prints them on stdout
;;;       as {"diagnostics":[…]} or a FHIR OperationOutcome instead (fhir/diagnostic).
;;;       --pack [out.tgz] also writes the FHIR NPM package (fsh/pack.rkt; default <id>-<version>.tgz):
;;;       package.json from sushi-config.yaml, conformance resources + #definition instances in
;;;       package/, other instances in package/example/ — loadable by `raco fhir serve-ig <tgz>`.
;;;       --fhir-version picks the target (default 5.0.0): 4.0.1 compiles an R4 IG against the R4 tower.
;;;       A sushi-config.yaml beside the sources (or one/two levels up) supplies canonical, fhirVersion
;;;       and dependencies (packages located in fhir-spec as <id>-<version>.tgz); flags override it.
;;;   raco fsh --decompile <file.json>...   the reverse direction (GoFSH-style): print FSH text
;;; Exit codes: 0 ok · 1 FSH error · 2 usage.

(require json
         (only-in racket/file make-directory*)
         (only-in racket/path path-only)
         (only-in fhir/sd/sd-index sd-index-available?)
         (only-in fsh/project compile-project pack-project! fsh-files project-missing project-diagnostics project-texts
                  project-artifacts project-config project-files)
         (only-in fsh/ig-config ig-config-id ig-config-version)
         (only-in fsh/decompile fhir->fsh)
         (only-in fhir/diagnostic emit-diagnostics format-flag))

(provide compile-path decompile-files)

;; compile-path : source out snapshot? canonical fhir-version -> void   compile (fsh/project.rkt),
;; report diagnostics (exit 1), optionally pack, write fsh-generated/
(define (compile-path src out snapshot? canonical [fhir-version #f] #:format [fmt 'text] #:pack [pack #f])
  (when (null? (fsh-files src)) (eprintf "fsh: no .fsh files at ~a\n" src) (exit 2))
  (when (and snapshot? (not (sd-index-available?)))
    (eprintf "fsh: --snapshot needs the R5 core package: raco pkg install fhir-spec\n") (exit 1))
  (define p (compile-project src #:canonical canonical #:fhir-version fhir-version #:snapshot? snapshot?))
  (for ([d (in-list (project-missing p))])
    (eprintf "fsh: dependency ~a ~a not found (looked for ~a-~a.tgz in fhir-spec); its profiles will not resolve\n"
             (car d) (cdr d) (car d) (cdr d)))
  (unless (null? (project-diagnostics p))
    (emit-diagnostics (project-diagnostics p) #:format fmt
                      #:text-of (lambda (s) (and s (hash-ref (project-texts p) s #f))))
    (exit 1))
  (define artifacts (project-artifacts p))
  (when pack
    (define cfg (project-config p))
    (unless cfg (eprintf "fsh: --pack needs the project's sushi-config.yaml (package id, version, dependencies)\n") (exit 2))
    (define tgz (pack-project! p (if (string? pack) pack
                                     (format "~a-~a.tgz" (or (ig-config-id cfg) "package") (or (ig-config-version cfg) "0.1.0")))))
    (printf "fsh: packed ~a resource(s) into ~a\n" (length artifacts) tgz))
  (define outdir
    (or out (build-path (if (directory-exists? src) src (or (path-only (car (project-files p))) (current-directory)))
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

(define (usage [code 2])                             ; --help: stdout, exit 0
  ((if (zero? code) printf eprintf) "usage: raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0] [--format text|json|outcome] [--pack [out.tgz]]\n       raco fsh --decompile <file.json>...\n")
  (exit code))

(module+ main
  (define-values (fmt args) (format-flag (vector->list (current-command-line-arguments))))
  ;; an unreadable input file is a one-line error (exit 2), not a stack trace
  (let ([default (uncaught-exception-handler)])
    (uncaught-exception-handler
     (lambda (e)
       (cond [(exn:fail:filesystem? e)
              (define m (regexp-match #px"path: ([^\n]*)" (exn-message e)))
              (eprintf "fsh: cannot read ~a\n" (if m (cadr m) (exn-message e)))
              (exit 2)]
             [else (default e)]))))
  (cond
    [(ormap (lambda (a) (member a '("-h" "--help"))) args) (usage 0)]
    [(null? args) (usage)]
    [(equal? (car args) "--decompile") (if (pair? (cdr args)) (decompile-files (cdr args)) (usage))]
    [else
     (define pack (let ([m (member "--pack" args)])
                    (and m (if (and (pair? (cdr m)) (regexp-match? #rx"[.]tgz$" (cadr m))) (cadr m) #t))))
     (let loop ([as (remove* (list "--pack" (and (string? pack) pack)) args)] [src #f] [out #f] [snapshot? #f] [canonical #f] [fv #f])
       (cond
         [(null? as) (if src (compile-path src out snapshot? canonical fv #:format fmt #:pack pack) (usage))]
         [(and (equal? (car as) "-o") (pair? (cdr as))) (loop (cddr as) src (cadr as) snapshot? canonical fv)]
         [(equal? (car as) "--snapshot") (loop (cdr as) src out #t canonical fv)]
         [(and (equal? (car as) "--canonical") (pair? (cdr as))) (loop (cddr as) src out snapshot? (cadr as) fv)]
         [(and (equal? (car as) "--fhir-version") (pair? (cdr as))) (loop (cddr as) src out snapshot? canonical (cadr as))]
         [(regexp-match? #rx"^-" (car as)) (usage)]
         [src (usage)]
         [else (loop (cdr as) (car as) out snapshot? canonical fv)]))]))
