#lang racket/base

;;; invariant-trace-test.rkt — an example that breaks a profile's invariant is reported at the rule
;;; that wrote the element the invariant found wrong, with the value (`… (found
;;; value.ofType(Quantity) = 300 '/min')`), not at the instance's header with the rule's words alone
;;; (a new user had to look for the value and the line themselves).

(require rackunit racket/file
         (only-in fsh/check check-fsh-project check-report-diagnostics)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line))

(define root (make-temporary-directory "fsh-invariant-~a"))
(with-output-to-file (build-path root "hr.fsh")
  (lambda ()
    (display #<<FSH
Profile: HeartRate
Parent: Observation
Id: heart-rate
* obeys hr-range

Invariant: hr-range
Description: "A heart rate is between 20 and 250 /min"
Severity: #error
Expression: "value.ofType(Quantity).where(value >= 20 and value <= 250).exists()"

Instance: fast
InstanceOf: HeartRate
* status = #final
* code = http://loinc.org#8867-4
* valueQuantity = 300 '/min'
FSH
             )))
(define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics (check-fsh-project root))))
(check-equal? (length errors) 1)
(check-regexp-match #rx"instance fast: Observation: hr-range: A heart rate is between 20 and 250 /min [(]found value.ofType[(]Quantity[)] = 300 '/min'[)]"
                    (diagnostic-message (car errors)))
(check-equal? (loc-line (diagnostic-loc (car errors))) 15)          ; `* valueQuantity = 300 '/min'`
(delete-directory/files root)
