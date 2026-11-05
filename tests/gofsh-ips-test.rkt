#lang racket/base

;;; gofsh-ips-test.rkt — the decompiler's real oracle: every resource of the PUBLISHED International
;;; Patient Summary 2.0.1 package (profiles, extensions, value sets, examples, the capability
;;; statement…) is decompiled to FSH (`fhir->fsh`, the GoFSH direction), the FSH is compiled back,
;;; and the result is held to the package by the same harness the IG's own FSH is held to
;;; (ig-harness.rkt: pairs, exact, with all published content, differential elements, and the
;;; difference classes as the work list). Needs the published package from `fhir-spec`; skipped
;;; with a note when it is not installed.
;;;
;;; RATCHETED — floors only rise. Out of scope: the ImplementationGuide resource (sushi-config).

(require rackunit racket/file racket/list racket/string
         (only-in fhir/r4/schemas)                         ; IPS is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/package read-package)
         fsh/decompile
         "ig-harness.rkt")

(define published (spec-package-path "hl7.fhir.uv.ips.tgz" #:optional? #t))
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))

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
  [(not (and published r4-core))
   (printf "  GoFSH IPS oracle: skipped (needs the fhir-spec package: raco pkg install fhir-spec)\n")]
  [else
   (define dir (make-temporary-file "gofsh-ips-~a" 'directory))
   (define-values (n failures) (decompile-package published dir))
   (printf "  GoFSH IPS 2.0.1: ~a resources decompiled, ~a failed\n" n (length failures))
   (for ([f (in-list failures)]) (eprintf "     decompile ~a: ~a\n" (car f) (cdr f)))
   (define r (parameterize ([current-sd-packages (list r4-core)])
               (check-fsh-ig dir published "http://hl7.org/fhir/uv/ips")))
   (printf "  GoFSH IPS 2.0.1 recompiled: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
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
   (check-true (>= (ig-fsh-report-pairs r) 117))
   (check-true (>= (ig-fsh-report-exact r) 116) "fewer exact resources than the floor")
   (check-true (>= (ig-fsh-report-contained r) 116) "fewer fully-matching resources than the floor")
   (check-true (>= (ig-fsh-report-elements-ok r) 438) "fewer matching differential elements than the floor")])
