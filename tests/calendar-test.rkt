#lang racket/base

;;; calendar-test.rkt — a date the calendar does not have is an error at the rule that wrote it
;;; (a new user's `* birthDate = "1940-02-30"` passed `raco fsh check` and `raco fhir-validate`, and
;;; CQL then computed an age from it). The spec's regex allows day 31 in every month; the validator
;;; now checks the day against its month (leap years by 4/100/400).

(require rackunit racket/file racket/list
         (only-in fsh/check check-fsh-project check-report-diagnostics)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line))

(define root (make-temporary-directory "fsh-calendar-~a"))
(with-output-to-file (build-path root "p.fsh")
  (lambda () (display (string-append "Instance: feb30\nInstanceOf: Patient\n* gender = #female\n"
                                     "* birthDate = \"1940-02-30\"\n\n"
                                     "Instance: leap\nInstanceOf: Patient\n* birthDate = \"2000-02-29\"\n"))))
(define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error))
                       (check-report-diagnostics (check-fsh-project root))))
(check-equal? (length errors) 1 (format "~s" (map diagnostic-message errors)))
(check-regexp-match #rx"'1940-02-30' is not a valid date [(]February 1940 has 29 days[)]"
                    (diagnostic-message (first errors)))
(check-equal? (loc-line (diagnostic-loc (first errors))) 4)       ; the rule, not the header
