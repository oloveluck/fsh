#lang racket/base

;;; check-r4-test.rkt — `raco fsh check` (fsh/check): every problem in an FSH project,
;;; positioned in the FSH. A small project with three planted mistakes must report exactly those, each
;;; at the rule (or instance) that makes it and naming the profile rule it breaks; mCODE 4.0.0's own
;;; sources must check clean (every example against every profile, and the generated server's
;;; requirements for its sender CapabilityStatement).

(require rackunit racket/runtime-path racket/list racket/string
         (only-in fhir/spec-package spec-package-path)
         fhir/diagnostic
         fsh/check)

(define-runtime-path bad "inputs/fsh-check-bad")
(define-runtime-path mcode "vendor/mcode-4.0.0")

(cond
  [(not (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
   (printf "fsh-check: skipped (needs fhir-spec)\n")]
  [else
   ;; ---- planted mistakes ----------------------------------------------------------------------
   (define r (check-fsh-project bad))
   (define ds (check-report-diagnostics r))
   (define (at d) (let ([l (diagnostic-loc d)])
                    (list (let-values ([(_d f _b) (split-path (loc-source l))]) (path->string f)) (loc-line l) (loc-column l))))
   (check-equal? (map at ds) '(("examples.fsh" 1 1) ("examples.fsh" 5 3) ("examples.fsh" 9 3)))
   (check-equal? (map diagnostic-phase ds) '(validate validate validate))
   ;; tumor-1 has no `subject` rule: the instance header, and the profile's `* subject 1..1`
   (check-regexp-match #rx"subject: minimum required = 1.*profile TumorSize at .*profiles.fsh:4:3" (diagnostic-message (first ds)))
   ;; a String where the slice allows only Quantity: the rule that wrote it, and the profile's `only`
   (check-regexp-match #rx"valueString is not allowed in slice 'longest'.*profiles.fsh:10:3" (diagnostic-message (second ds)))
   ;; a status outside its required value set: the rule (the base, not the profile, constrains it)
   (check-regexp-match #rx"status: code `[^`]+` not in required value set" (diagnostic-message (third ds)))
   (check-equal? (list (check-report-examples r) (check-report-invalid r)) '(2 2))
   ;; ---- a real IG checks clean ------------------------------------------------------------------
   (define m (check-fsh-project mcode #:capability "mcode-sender-patients-in-group"))
   (check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics m)) '())
   (printf "  fsh check: mCODE 4.0.0 ~a examples, ~a with errors, ~a diagnostics\n"
           (check-report-examples m) (check-report-invalid m) (length (check-report-diagnostics m)))])
