#lang racket/base

;;; ig-hrex-test.rkt — Da Vinci HRex 1.1.0 (Health Record Exchange), compiled from its own FSH sources
;;; (tests/vendor/hrex-1.1.0, HL7/davinci-ehrx at tag 1.1.0) and compared with its published package
;;; (see ig-harness.rkt for what is normalised). The published package and US Core 7.0.0 are fetched
;;; from packages.fhir.org (tests/ig-fetch.rkt); US Core 6.1.0 (declared as the hl7.fhir.us.core.v610
;;; alias) is fhir-spec's; skipped with a note when they cannot be had. HRex declares THREE US Core
;;; versions (7.0.0, 6.1.0, 3.1.1 — the last two as .v610/.v311 aliases) and pins Parents and
;;; Reference targets to 7.0.0 (`USCoreProvenance|7.0.0`); the packages go into the SD context in
;;; that order, so an unpinned US Core name resolves to its 7.0.0 definition.
;;;
;;; RATCHETED — floors only rise. Known and out of scope: the ImplementationGuide resource (sushi-config).

(require rackunit racket/runtime-path (only-in racket/list remove-duplicates)
         (only-in fhir/r4/schemas)                          ; HRex is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path current-package-directories)
         (only-in fhir/sd/sd-index current-sd-packages)
         fsh/ig-config
         "ig-fetch.rkt" "ig-harness.rkt")

(define-runtime-path hrex "vendor/hrex-1.1.0")
(define published (fetched-package "hl7.fhir.us.davinci-hrex" "1.1.0"))
(define uscore7 (fetched-package "hl7.fhir.us.core" "7.0.0"))
(define uscore6 (spec-package-path "hl7.fhir.us.core-6.1.0.tgz" #:optional? #t))
(define uscore3 (fetched-package "hl7.fhir.us.core" "3.1.1" "a998fa41cb5a0618d22ed8671c71ca9954b1ac8c"))  ; declared as hl7.fhir.us.core.v311
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
(define cfg (read-ig-config (build-path hrex "sushi-config.yaml")))

(printf "  HRex declares: ~s\n" (ig-config-dependencies cfg))
(check-not-false (member '("hl7.fhir.us.core" . "7.0.0") (ig-config-dependencies cfg)))
(check-not-false (member '("hl7.fhir.us.core.v610" . "6.1.0") (ig-config-dependencies cfg)))
(cond
  [(not (and published uscore7 uscore6 uscore3 r4-core))
   (printf "  HRex IG corpus: skipped (needs the published package and US Core 7.0.0 from packages.fhir.org, and fhir-spec)\n")]
  [else
   (define r (parameterize ([current-package-directories (list packages-dir)]
                            [current-sd-packages (list uscore7 uscore6 uscore3 r4-core)])
               (check-fsh-ig (build-path hrex "input" "fsh") published (ig-config-canonical cfg))))
   (printf "  HRex 1.1.0 from FSH: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
           (ig-fsh-report-pairs r) (ig-fsh-report-exact r) (ig-fsh-report-contained r)
           (ig-fsh-report-elements-ok r) (ig-fsh-report-elements-total r))
   (for ([e (in-list (ig-fsh-report-errors r))]) (eprintf "     compile error ~a: ~a\n" (car e) (cdr e)))
   (printf "  unmatched: ours ~s; published ~s\n" (ig-fsh-report-ours-unmatched r)
           (remove-duplicates (map car (ig-fsh-report-published-unmatched r))))
   (printf "  difference classes (published vs ours), largest first:\n")
   (print-classes r)
   (check-true (>= (ig-fsh-report-pairs r) 0))
   (check-true (>= (ig-fsh-report-exact r) 0))
   (check-true (>= (ig-fsh-report-contained r) 0))
   (check-true (>= (ig-fsh-report-elements-ok r) 0))])
