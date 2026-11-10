#lang racket/base

;;; duplicate-id-test.rkt — two instances with one id in two files: the duplicate is reported, and each
;;; instance's own problems stay at its own file and line (both went to the later file, the earlier
;;; never named).

(require rackunit racket/file racket/list racket/string
         (only-in fsh/check check-fsh-project check-report-diagnostics)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line loc-source))

(define root (make-temporary-directory "fsh-dup-~a"))
(make-directory* (build-path root "a"))
(make-directory* (build-path root "b"))
(with-output-to-file (build-path root "a" "bad1.fsh")
  (lambda () (display "Instance: intake-bad\nInstanceOf: Patient\n* gender = #femme\n")))
(with-output-to-file (build-path root "b" "bad2.fsh")
  (lambda () (display "// b\n\nInstance: intake-bad\nInstanceOf: Patient\n* birthDate = \"1950-13-01\"\n")))
(define ds (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics (check-fsh-project root))))
(define (at d) (format "~a:~a" (let ([s (loc-source (diagnostic-loc d))]) (if s (let-values ([(_d n _x) (split-path s)]) (path->string n)) "?"))
                       (loc-line (diagnostic-loc d))))
(define (find rx) (findf (lambda (d) (regexp-match? rx (diagnostic-message d))) ds))
(check-not-false (find #rx"already used"))
(check-equal? (at (find #rx"femme")) "bad1.fsh:3")
(check-equal? (at (find #rx"1950-13-01")) "bad2.fsh:5")
(check-equal? (length (filter (lambda (d) (regexp-match? #rx"femme" (diagnostic-message d))) ds)) 1)
(delete-directory/files root)
