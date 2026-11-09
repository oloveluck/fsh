#lang racket/base

;;; lang-check-test.rkt — a `#lang fsh` module whose profile constrains an element the base type does
;;; not have does not compile (a new user's `* birthdate 1..1` built, and every valid Patient then
;;; failed validation, blaming the data); one that is right compiles.

(require rackunit racket/file (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-message diagnostic-loc loc-line))

(define dir (make-temporary-directory "fsh-lang-~a"))
(define (module-at name text)
  (define f (build-path dir name))
  (void (call-with-output-file f (lambda (o) (write-string text o))))
  f)
(define bad (module-at "bad.fsh" "#lang fsh\nProfile: IntakePatient\nParent: Patient\n* name 1..*\n* birthdate 1..1\n"))
(define good (module-at "good.fsh" "#lang fsh\nProfile: IntakePatient\nParent: Patient\n* name 1..*\n* birthDate 1..1\n"))

(check-exn (lambda (e) (and (exn:fail:diagnostic? e)
                            (let ([d (exn:fail:diagnostic-diagnostic e)])
                              (and (regexp-match? #rx"`birthdate` is not an element of Patient [(]did you mean `birthDate`[?][)]" (diagnostic-message d))
                                   (equal? (loc-line (diagnostic-loc d)) 5)))))
           (lambda () (dynamic-require bad #f)))
(check-regexp-match #rx"/StructureDefinition/IntakePatient$" ((dynamic-require good 'canonical-of) "IntakePatient"))
(delete-directory/files dir)
