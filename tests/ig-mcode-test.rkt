#lang racket/base

;;; ig-mcode-test.rkt — mCODE 4.0.0 (minimal Common Oncology Data Elements), compiled from its own FSH
;;; sources (tests/vendor/mcode-4.0.0, the HL7/fhir-mCODE-ig repo at tag 4.0.0) and compared with its
;;; published package (see ig-harness.rkt for what is normalised).
;;;
;;; The second real FSH IG, and the first with IG DEPENDENCIES: its sushi-config.yaml names US Core
;;; 6.1.0 and Genomics Reporting 2.0.0, read by fsh/ig-config and put ahead of R4 core in
;;; current-sd-packages, so `Parent: USCoreObservationSimple` and `InstanceOf:` a genomics profile
;;; resolve against those packages (base type, canonical, required pattern values). Needs fhir-spec;
;;; skipped with a note otherwise.
;;;
;;; RATCHETED — floors only rise. Known and out of scope: the ImplementationGuide resource (generated
;;; by SUSHI/the publisher from sushi-config.yaml).

(require rackunit racket/runtime-path
         (only-in fhir/r4/schemas)                          ; mCODE is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         fsh/ig-config
         "ig-harness.rkt")

(define-runtime-path mcode "vendor/mcode-4.0.0")
(define published (spec-package-path "hl7.fhir.us.mcode-4.0.0.tgz" #:optional? #t))
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
(define cfg (read-ig-config (build-path mcode "sushi-config.yaml")))
(define-values (deps missing) (dependency-packages (ig-config-dependencies cfg)))

(check-equal? (ig-config-dependencies cfg)
              '(("hl7.fhir.uv.genomics-reporting" . "2.0.0") ("hl7.fhir.us.core" . "6.1.0")))
(cond
  [(not (and published r4-core (null? missing)))
   (printf "  mCODE IG corpus: skipped (needs the fhir-spec package with mCODE and its dependencies)\n")]
  [else
   (define r (parameterize ([current-sd-packages (append deps (list r4-core))])
               (check-fsh-ig (build-path mcode "fsh") published (ig-config-canonical cfg))))
   (printf "  mCODE 4.0.0 from FSH: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
           (ig-fsh-report-pairs r) (ig-fsh-report-exact r) (ig-fsh-report-contained r)
           (ig-fsh-report-elements-ok r) (ig-fsh-report-elements-total r))
   (for ([e (in-list (ig-fsh-report-errors r))]) (eprintf "     compile error ~a: ~a\n" (car e) (cdr e)))
   (check-equal? (ig-fsh-report-errors r) '())
   (check-equal? (ig-fsh-report-ours-unmatched r) '())
   (check-equal? (map car (ig-fsh-report-published-unmatched r)) '("ImplementationGuide"))
   (check-true (>= (ig-fsh-report-pairs r) 349))
   (check-true (>= (ig-fsh-report-exact r) 206) "fewer exact resources than the floor")
   (check-true (>= (ig-fsh-report-contained r) 282) "fewer fully-matching resources than the floor")
   (check-true (>= (ig-fsh-report-elements-ok r) 279) "fewer matching differential elements than the floor")])
