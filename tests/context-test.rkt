#lang racket/base

;;; context-test.rkt — examples meant to fail live in their own folder and are checked against the
;;; project's profiles with #:context (raco fsh check bad/ --context fsh/): the profile is in context,
;;; its own examples are not checked again, and nothing is a duplicate (the rerun's copy-the-profile
;;; layout reported duplicate ids).

(require rackunit racket/file
         (only-in fsh/check check-fsh-project check-report-diagnostics check-report-examples)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity))

(define root (make-temporary-directory "fsh-context-~a"))
(make-directory (build-path root "fsh"))
(make-directory (build-path root "bad"))
(with-output-to-file (build-path root "fsh" "intake.fsh")
  (lambda () (display "Profile: IntakePatient\nParent: Patient\nId: intake-patient\n* birthDate 1..1\n\nInstance: good\nInstanceOf: IntakePatient\n* birthDate = \"1950-05-01\"\n")))
(with-output-to-file (build-path root "bad" "bad.fsh")
  (lambda () (display "Instance: no-birthdate\nInstanceOf: IntakePatient\n* gender = #female\n")))

(define r (check-fsh-project (build-path root "bad") #:context (list (build-path root "fsh"))))
(define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics r)))
(check-equal? (check-report-examples r) 1)                       ; only the folder's own example
(check-equal? (length errors) 1)
(check-regexp-match #rx"instance no-birthdate: Patient.birthDate: minimum required = 1" (diagnostic-message (car errors)))
;; the project itself is clean, and the whole tree has no duplicate
(check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics (check-fsh-project (build-path root "fsh")))) '())
(check-equal? (length (filter (lambda (d) (regexp-match? #rx"already used" (diagnostic-message d))) (check-report-diagnostics (check-fsh-project root)))) 0)
(delete-directory/files root)
