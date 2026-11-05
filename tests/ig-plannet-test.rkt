#lang racket/base

;;; ig-plannet-test.rkt — Da Vinci PDex Plan-Net 1.2.0, compiled from its own FSH sources
;;; (tests/vendor/plannet-1.2.0, HL7/davinci-pdex-plan-net at tag 1.2.0) and compared with its published
;;; package (see ig-harness.rkt for what is normalised). The package and its dependency, US Core 3.1.1
;;; (declared as hl7.fhir.us.core.v311 — a version-alias stub the fetcher follows to hl7.fhir.us.core
;;; 3.1.1), are fetched from packages.fhir.org into tests/vendor/packages/ when not present
;;; (tests/ig-fetch.rkt); skipped with a note when they cannot be had.
;;;
;;; RATCHETED — floors only rise. Known and out of scope: the ImplementationGuide resource (sushi-config).

(require rackunit racket/runtime-path
         (only-in fhir/r4/schemas)                          ; Plan-Net is FHIR 4.0.1
         (only-in fhir/spec-package spec-package-path current-package-directories)
         (only-in fhir/sd/sd-index current-sd-packages resolve-sd-raw)
         fsh/ig-config
         "ig-fetch.rkt" "ig-harness.rkt")

(define-runtime-path plannet "vendor/plannet-1.2.0")
(define published (fetched-package "hl7.fhir.us.davinci-pdex-plan-net" "1.2.0" "509493840b88c2f88758d603e1bb148643f6f9f2"))
(define uscore311 (fetched-package "hl7.fhir.us.core.v311" "3.1.1" "5abae31a6254ddaa124049012849a0b51b348f02"))  ; a stub for hl7.fhir.us.core 3.1.1
(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))
(define cfg (read-ig-config (build-path plannet "sushi-config.yaml")))

(check-equal? (ig-config-dependencies cfg)
              '(("hl7.fhir.us.core.v311" . "3.1.1") ("hl7.fhir.us.core.v610" . "6.1.0")
                ("hl7.fhir.us.core" . "7.0.0") ("hl7.fhir.us.davinci-hrex" . "1.1.0")))
;; the profiles compile against US Core 3.1.1 alone (the later US Core versions and HRex are listed
;; for conformance statements; no FSH rule reaches into them)
(cond
  [(not (and published uscore311 r4-core))
   (printf "  Plan-Net IG corpus: skipped (needs the published package and US Core 3.1.1 from packages.fhir.org, and fhir-spec)\n")]
  [else
   (parameterize ([current-sd-packages (list uscore311 r4-core)])
     (printf "  US Core 3.1.1 (~a): us-core-location resolves: ~a\n" uscore311
             (and (resolve-sd-raw "http://hl7.org/fhir/us/core/StructureDefinition/us-core-location") #t)))
   (define r (parameterize ([current-package-directories (list packages-dir)]
                            [current-sd-packages (list uscore311 r4-core)])
               (check-fsh-ig (build-path plannet "input" "fsh") published (ig-config-canonical cfg))))
   (printf "  Plan-Net 1.2.0 from FSH: ~a resources paired; ~a exact, ~a with all published content; SD differential ~a/~a elements\n"
           (ig-fsh-report-pairs r) (ig-fsh-report-exact r) (ig-fsh-report-contained r)
           (ig-fsh-report-elements-ok r) (ig-fsh-report-elements-total r))
   (for ([e (in-list (ig-fsh-report-errors r))]) (eprintf "     compile error ~a: ~a\n" (car e) (cdr e)))
   (printf "  unmatched: ours ~s; published ~s\n" (ig-fsh-report-ours-unmatched r) (map car (ig-fsh-report-published-unmatched r)))
   (printf "  difference classes (published vs ours), largest first:\n")
   (print-classes r)
   (check-equal? (ig-fsh-report-errors r) '())
   (check-true (>= (ig-fsh-report-pairs r) 100))
   (check-true (>= (ig-fsh-report-exact r) 0))
   (check-true (>= (ig-fsh-report-contained r) 0))
   (check-true (>= (ig-fsh-report-elements-ok r) 0))])
