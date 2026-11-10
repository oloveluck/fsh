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
(check-regexp-match #rx"instance no-birthdate: Patient.birthDate: minimum required = 1, but found 0 [(]no rule of no-birthdate writes `birthDate`[)]" (diagnostic-message (car errors)))
;; the project itself is clean, and the whole tree has no duplicate
(check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics (check-fsh-project (build-path root "fsh")))) '())
(check-equal? (length (filter (lambda (d) (regexp-match? #rx"already used" (diagnostic-message d))) (check-report-diagnostics (check-fsh-project root)))) 0)
;; a value the profile does not constrain (a core binding, a date's format) cites no profile rule;
;; a cardinality the profile sets does (it cited `* birthDate 1..1` for "femme")
(with-output-to-file (build-path root "bad" "bad.fsh") #:exists 'truncate
  (lambda () (display "Instance: femme\nInstanceOf: IntakePatient\n* gender = #femme\n* birthDate = \"1950-13-01\"\n")))
(define ms (map diagnostic-message (filter (lambda (d) (eq? (diagnostic-severity d) 'error))
                                           (check-report-diagnostics (check-fsh-project (build-path root "bad") #:context (list (build-path root "fsh")))))))
(check-equal? (length ms) 2)
(check-false (ormap (lambda (m) (regexp-match? #rx"constrained by profile" m)) ms) (format "~s" ms))
(check-not-false (regexp-match #rx"constrained by profile IntakePatient" (diagnostic-message (car errors))))
(delete-directory/files root)
