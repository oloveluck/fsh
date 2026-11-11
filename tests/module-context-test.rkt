#lang racket/base
;;; A `#lang fsh` file of examples whose profile is in another folder (a new user's bad-examples/
;;; beside profiles/): run alone it fails at the instance's header, one line, saying where the profile
;;; is and what to do (it was a stack trace through sd.rkt); required after the profile's module it
;;; builds, the profile found in fhir/registry by name (InstanceOf: IntakePatient).
(require rackunit racket/file)

(define d (make-temporary-directory "fsh-module-context-~a"))
(define (put! name text) (call-with-output-file (build-path d name) (lambda (o) (void (write-string text o)))))
(make-directory* (build-path d "profiles"))
(make-directory* (build-path d "bad"))
(put! "profiles/intake.fsh" "#lang fsh\nProfile: IntakePatient\nParent: Patient\nId: intake-patient\n* birthDate 1..1\n")
(put! "bad/bad.fsh" "#lang fsh\nInstance: bad-patient\nInstanceOf: IntakePatient\nUsage: #example\n* gender = #female\n")

(define (fresh thunk) (parameterize ([current-namespace (make-base-namespace)]) (thunk)))

;; alone: a positioned user error with the hint
(fresh
 (lambda ()
   (check-exn (lambda (e)
                (and (exn:fail:user? e)
                     (regexp-match? #rx"bad[.]fsh:2:1: .*InstanceOf `IntakePatient` not found.*`IntakePatient` is defined in .*profiles/intake[.]fsh: require that module before this one, or check this folder with `raco fsh check .*bad --context .*profiles`"
                                    (exn-message e))))
              (lambda () (dynamic-require (build-path d "bad" "bad.fsh") 'compiled)))))

;; after the profile's module: the instance claims the profile's url
(fresh
 (lambda ()
   (dynamic-require (build-path d "profiles" "intake.fsh") #f)
   (define compiled (dynamic-require (build-path d "bad" "bad.fsh") 'compiled))
   (check-equal? (hash-ref (hash-ref (car compiled) 'meta) 'profile)
                 '("http://example.org/StructureDefinition/intake-patient"))))

(delete-directory/files d)
