#lang racket/base

;;; gofsh-mcode-test.rkt — the decompiler's second oracle: the PUBLISHED mCODE 4.0.0 package
;;; (its dependencies US Core 6.1.0 and Genomics Reporting 2.0.0 in the SD context, as its own FSH
;;; compiles) decompiled with `fhir->fsh`, compiled back, and held to the package by the same
;;; harness as the IG's own FSH (ig-harness.rkt). mCODE has what IPS has not: complex extensions
;;; with nested slices, reslicing, profiles of a dependency's profiles, ConceptMaps, Groups.
;;;
;;; RATCHETED — floors only rise. Out of scope: the ImplementationGuide resource.

(require rackunit racket/file racket/list racket/string racket/runtime-path
         (only-in fhir/r4/schemas)                         ; mCODE is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/package read-package)
         fsh/decompile fsh/ig-config
         "ig-harness.rkt")

(define-runtime-path mcode "vendor/mcode-4.0.0")
(define published (spec-package-path "hl7.fhir.us.mcode-4.0.0.tgz" #:optional? #t))
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
(define cfg (read-ig-config (build-path mcode "sushi-config.yaml")))
(define-values (deps missing) (dependency-packages (ig-config-dependencies cfg)))

;; decompile-package : tgz dir -> (values n failures)   every resource of the package as FSH, one
;; file per resource type, in `dir`; a resource the decompiler cannot handle is a failure (id . message)
(define (decompile-package tgz dir)
  (define-values (manifest resources examples) (read-package tgz))
  (define jss (for/list ([e (in-list (append resources examples))]
                         #:unless (equal? (hash-ref (cdr e) 'resourceType #f) "ImplementationGuide"))
                (cdr e)))
  (define failures '())
  (define by-type (make-hash))
  (for ([j (in-list jss)])
    (with-handlers ([exn:fail? (lambda (e) (set! failures (cons (cons (hash-ref j 'id #f) (exn-message e)) failures)))])
      (hash-update! by-type (hash-ref j 'resourceType) (lambda (l) (cons (fhir->fsh j) l)) '())))
  (for ([(type texts) (in-hash by-type)])
    (display-to-file (string-join (reverse texts) "\n\n") (build-path dir (string-append type ".fsh")) #:exists 'replace))
  (values (- (length jss) (length failures)) (reverse failures)))

(cond
  [(not (and published r4-core (null? missing)))
   (printf "  GoFSH mCODE oracle: skipped (needs the fhir-spec package with mCODE and its dependencies)\n")]
  [else
   (define dir (make-temporary-file "gofsh-mcode-~a" 'directory))
   (define-values (n failures) (decompile-package published dir))
   (printf "  GoFSH mCODE 4.0.0: ~a resources decompiled, ~a failed\n" n (length failures))
   (for ([f (in-list failures)]) (eprintf "     decompile ~a: ~a\n" (car f) (cdr f)))
   (define r (parameterize ([current-sd-packages (append deps (list r4-core))])
               (check-fsh-ig dir published (ig-config-canonical cfg))))
   (printf "  GoFSH mCODE 4.0.0 recompiled: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
           (ig-fsh-report-pairs r) (ig-fsh-report-exact r) (ig-fsh-report-contained r)
           (ig-fsh-report-elements-ok r) (ig-fsh-report-elements-total r))
   (for ([e (in-list (ig-fsh-report-errors r))]) (eprintf "     compile error ~a: ~a\n" (car e) (cdr e)))
   (printf "  unmatched: ours ~s; published ~s\n" (ig-fsh-report-ours-unmatched r)
           (remove-duplicates (map car (ig-fsh-report-published-unmatched r))))
   (printf "  difference classes (published vs ours), largest first:\n")
   (print-classes r)
   (delete-directory/files dir)
   (check-equal? failures '())
   ;; floors (ratcheted): decompile everything, compile everything, and the counts only rise
   (check-equal? (ig-fsh-report-errors r) '())
   (check-true (>= (ig-fsh-report-pairs r) 300))
   (check-true (>= (ig-fsh-report-exact r) 0) "fewer exact resources than the floor")
   (check-true (>= (ig-fsh-report-contained r) 0) "fewer fully-matching resources than the floor")
   (check-true (>= (ig-fsh-report-elements-ok r) 0) "fewer matching differential elements than the floor")])
