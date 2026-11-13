#lang racket/base

;;; own-binding-test.rkt — a profile's OWN required value set is enforced on the project's examples.
;;; `* gender from BinarySex (required)` decided nothing (the validator knew only the value sets
;;; generated with the type tower), so `#other` passed and only a code outside FHIR's own set was
;;; refused, citing FHIR's set. And a dependency no package provides is in the report.

(require rackunit racket/file
         (only-in fsh/check check-fsh-project check-report-diagnostics check-report-missing)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line))

(define dir (make-temporary-file "fsh-own-binding-~a" 'directory))
(with-output-to-file (build-path dir "b.fsh")
  (lambda ()
    (display (string-append
              "ValueSet: BinarySex\nId: binary-sex\n"
              "* http://hl7.org/fhir/administrative-gender#female\n"
              "* http://hl7.org/fhir/administrative-gender#male\n\n"
              "Profile: StudyPatient\nParent: Patient\n* gender 1..1\n* gender from BinarySex (required)\n\n"
              "Instance: fine\nInstanceOf: StudyPatient\n* gender = #female\n\n"
              "Instance: other-p\nInstanceOf: StudyPatient\n* gender = #other\n"))))   ; line 17: the rule
(define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics (check-fsh-project dir))))
(check-equal? (length errors) 1)
(check-regexp-match #rx"instance other-p: Patient.gender: code `other` not in required value set http://example.org/ValueSet/binary-sex; it holds female, male"
                    (diagnostic-message (car errors)))
(check-equal? (loc-line (diagnostic-loc (car errors))) 17)

;; a dependency nothing provides is named in the report (`raco fsh check` prints it)
(with-output-to-file (build-path dir "sushi-config.yaml")
  (lambda () (display "id: x.test\ncanonical: http://example.org\nversion: 0.1.0\nfhirVersion: 4.0.1\ndependencies:\n  no.such.package: 1.2.3\n")))
(check-equal? (check-report-missing (check-fsh-project dir)) '(("no.such.package" . "1.2.3")))
(delete-directory/files dir)
