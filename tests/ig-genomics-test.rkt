#lang racket/base

;;; ig-genomics-test.rkt — the fifth real-IG corpus: HL7 Genomics Reporting 2.0.0's own FSH
;;; (tests/vendor/genomics-reporting-2.0.0, see its SOURCE.txt) compiled for R4 and compared with
;;; the published package (fhir-spec's hl7.fhir.uv.genomics-reporting-2.0.0.tgz — also a dependency
;;; of mCODE). What it adds to the other four: OperationDefinitions and ConceptMaps as instances,
;;; deep slicing of Observation components with RuleSets, code systems with many concepts.
;;;
;;; RATCHETED — floors only rise. Out of scope: the ImplementationGuide resource.

(require rackunit racket/runtime-path racket/list
         (only-in fhir/r4/schemas)                          ; Genomics Reporting is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         fsh/ig-config
         "ig-harness.rkt")

(define-runtime-path genomics "vendor/genomics-reporting-2.0.0")
(define published (spec-package-path "hl7.fhir.uv.genomics-reporting-2.0.0.tgz" #:optional? #t))
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
(define cfg (read-ig-config (build-path genomics "sushi-config.yaml")))

(cond
  [(not (and published r4-core))
   (printf "  Genomics Reporting IG corpus: skipped (needs the fhir-spec package)\n")]
  [else
   (define r (parameterize ([current-sd-packages (list r4-core)])
               (check-fsh-ig (build-path genomics "fsh") published (ig-config-canonical cfg))))
   (printf "  Genomics Reporting 2.0.0 from FSH: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
           (ig-fsh-report-pairs r) (ig-fsh-report-exact r) (ig-fsh-report-contained r)
           (ig-fsh-report-elements-ok r) (ig-fsh-report-elements-total r))
   (for ([e (in-list (ig-fsh-report-errors r))]) (eprintf "     compile error ~a: ~a\n" (car e) (cdr e)))
   (printf "  unmatched: ours ~s; published ~s\n" (ig-fsh-report-ours-unmatched r)
           (remove-duplicates (map car (ig-fsh-report-published-unmatched r))))
   (printf "  difference classes (published vs ours), largest first:\n")
   (print-classes r)
   ;; RATCHETED floors
   (check-true (>= (ig-fsh-report-pairs r) 141))
   (check-true (>= (ig-fsh-report-exact r) 121) "fewer exact resources than the floor")
   (check-true (>= (ig-fsh-report-contained r) 129) "fewer fully-matching resources than the floor")
   (check-true (>= (ig-fsh-report-elements-ok r) 254) "fewer matching differential elements than the floor")])
