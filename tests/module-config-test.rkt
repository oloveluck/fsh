#lang racket/base

;;; module-config-test.rkt — a `#lang fsh` module compiles as `raco fsh` does: with the nearest
;;; sushi-config.yaml's canonical and FHIR version, and says a profile's url by name (canonical-of).

(require rackunit racket/file)

(define dir (make-temporary-directory "fsh-module-~a"))
(call-with-output-file (build-path dir "sushi-config.yaml")
  (lambda (o) (void (write-string "id: intake\ncanonical: http://example.org/intake\nfhirVersion: 4.0.1\nname: Intake\nstatus: draft\nversion: 0.1.0\n" o))))
(make-directory (build-path dir "input"))
(define src (build-path dir "input" "intake.fsh"))
(call-with-output-file src
  (lambda (o) (void (write-string "#lang fsh\nProfile: IntakePatient\nParent: Patient\nId: intake-patient\n* birthDate 1..1\n" o))))
(define compiled (dynamic-require src 'compiled))
(define canonical-of (dynamic-require src 'canonical-of))
(define sd (car compiled))
(check-equal? (hash-ref sd 'url) "http://example.org/intake/StructureDefinition/intake-patient")
(check-equal? (hash-ref sd 'fhirVersion) "4.0.1")
(check-equal? (canonical-of "IntakePatient") "http://example.org/intake/StructureDefinition/intake-patient")
(check-equal? (canonical-of "intake-patient") "http://example.org/intake/StructureDefinition/intake-patient")
(check-exn #rx"defines nothing of that name" (lambda () (canonical-of "Nope")))
(delete-directory/files dir)
