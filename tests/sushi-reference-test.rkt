#lang racket/base

;;; sushi-reference-test.rkt — what running this compiler beside SUSHI 3.21.1 on SUSHI's own test
;;; inputs, and on copies changed in one rule, found (fhir-examples' value-experiment, H part 3):
;;; resources written differently, and errors SUSHI reports that this compiler did not. Each case
;;; fails without its fix. The texts are compiled as a project of one file with a sushi-config.yaml
;;; (FHIR 4.0.1, FSHOnly), as `raco fsh <dir>` compiles.

(require rackunit racket/file racket/list racket/string
         (only-in "../project.rkt" compile-project project-artifacts project-diagnostics)
         (only-in fhir/diagnostic diagnostic-severity diagnostic-message))

(define work (make-temporary-file "fsh-sushi-reference-~a" 'directory))
(display-to-file "canonical: http://example.org\nfhirVersion: 4.0.1\nFSHOnly: true\nversion: 1.0.0\n" (build-path work "sushi-config.yaml"))

;; compile : string -> (values (listof jsexpr) (listof string))   the resources and the error messages
(define (compile text)
  (display-to-file text (build-path work "input.fsh") #:exists 'truncate)
  (define p (compile-project work))
  (values (project-artifacts p)
          (for/list ([d (in-list (project-diagnostics p))] #:when (eq? (diagnostic-severity d) 'error)) (diagnostic-message d))))
(define (resources text) (let-values ([(rs errors) (compile text)]) (check-equal? errors '() text) rs))
(define (errors text) (let-values ([(_rs errors) (compile text)]) errors))
(define (elements sd) (hash-ref (hash-ref sd 'differential (hasheq)) 'element '()))
(define (element sd id) (findf (lambda (e) (equal? (hash-ref e 'id) id)) (elements sd)))

;; ---- what is written --------------------------------------------------------------------------
;; a configuration that states no status means draft (SUSHI's command line; it was `active`)
(check-equal? (hash-ref (car (resources "Profile: P\nParent: Patient\n")) 'status) "draft")
(check-equal? (hash-ref (car (resources "ValueSet: V\n")) 'status) "draft")
(check-equal? (hash-ref (car (resources "CodeSystem: C\n* #a\n")) 'status) "draft")
;; a profile that changes nothing has its root element as its differential (it had none)
(check-equal? (elements (car (resources "Profile: P\nParent: Observation\n"))) (list (hasheq 'id "Observation" 'path "Observation")))
;; an extension that says nothing of its value or sub-extensions fixes its url and no more
(check-equal? (map (lambda (e) (hash-ref e 'id)) (elements (car (resources "Extension: E\n")))) '("Extension.url"))
(check-equal? (map (lambda (e) (hash-ref e 'id)) (elements (car (resources "Extension: E\n* value[x] only boolean\n"))))
              '("Extension.extension" "Extension.url" "Extension.value[x]"))
;; a value set that includes nothing has no compose (it was `{}`)
(check-false (hash-has-key? (car (resources "ValueSet: V\n")) 'compose))
;; a concept named again, to set a caret under it, keeps its display
(let ([cs (car (resources "CodeSystem: ZOO\n* #anteater \"Anteater\"\n* #anteater\n  * ^property[0].valueString = \"cute\"\n"))])
  (check-equal? (hash-ref (car (hash-ref cs 'concept)) 'display) "Anteater"))
;; TU, N and D are the element's standards status (they were read and dropped) — also where the
;; base element already carries that status (Observation.focus is trial-use in R4)
(let ([sd (car (resources "Profile: P\nParent: Observation\n* category 1..5 MS TU\n* value[x] 1..1 N\n* component 2..* D\n* focus ?! TU\n"))])
  (define (status id) (for/or ([x (in-list (hash-ref (element sd id) 'extension '()))]) (hash-ref x 'valueCode #f)))
  (check-equal? (map status '("Observation.category" "Observation.value[x]" "Observation.component" "Observation.focus"))
                '("trial-use" "normative" "draft" "trial-use"))
  (check-equal? (hash-ref (car (hash-ref (element sd "Observation.category") 'extension)) 'url)
                "http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status")
  (check-false (hash-has-key? (car (hash-ref (element sd "Observation.category") 'extension)) 'stated)))
;; a Ratio assigned in a profile (it raised: a ratio was no value the compiler knew)
(let ([sd (car (resources "Profile: P\nParent: Observation\n* valueRatio = 130 'mg' : 1 'dL'\n"))])
  (define ratio (for/or ([e (in-list (elements sd))]) (hash-ref e 'patternRatio #f)))
  (check-equal? (hash-ref (hash-ref ratio 'numerator) 'value) 130)
  (check-equal? (hash-ref (hash-ref ratio 'denominator) 'code) "dL"))

;; a unit with a display and no number (`'[lb_av]' "lb"`) is the unit's code and display (the
;; unit was written as the value, the display as the code)
(let ([sd (car (resources "Profile: P\nParent: Observation\n* valueQuantity = '[lb_av]' \"lb\"\n"))])
  (define q (for/or ([e (in-list (elements sd))]) (hash-ref e 'patternQuantity #f)))
  (check-equal? (hash-ref q 'code) "[lb_av]")
  (check-equal? (hash-ref q 'unit) "lb")
  (check-false (hash-has-key? q 'value)))

;; ---- what is refused ----------------------------------------------------------------------------
;; a binding to a name that is no value set (it was a warning)
(check-regexp-match #rx"value set `CategoryValueSet` not found"
                    (car (errors "Profile: P\nParent: Observation\n* category from CategoryValueSet (required)\n")))
;; a code whose system is no code system, where the value keeps the system
(check-regexp-match #rx"`foo` in `foo#bar` is no code system"
                    (car (errors "Profile: P\nParent: Observation\n* code = foo#bar\n")))
(check-equal? (errors "Alias: $loinc = http://loinc.org\nProfile: P\nParent: Observation\n* code = $loinc#1234-5\n* status = Whatever#final\n") '())
(check-regexp-match #rx"`ZOO` is no code system" (car (errors "ValueSet: V\n* ZOO#bear\n")))
(check-equal? (errors "CodeSystem: ZOO\n* #bear\n\nValueSet: V\n* ZOO#bear\n") '())
;; an id that is no FHIR id
(check-regexp-match #rx"is not a FHIR id" (car (errors "Instance: 2000-10-31T00:01:02.000+00:00\nInstanceOf: Patient\n")))
(check-equal? (errors "Instance: ann-1.2\nInstanceOf: Patient\n") '())
;; a cardinality that does not fit within the base type's
(check-regexp-match #rx"cannot constrain `value\\[x\\]` to 1[.][.][*]: it does not fit within Observation's 0[.][.]1"
                    (car (errors "Profile: P\nParent: Observation\n* value[x] 1..*\n")))
(check-regexp-match #rx"does not fit within Observation's 1[.][.]1" (car (errors "Profile: P\nParent: Observation\n* status 0..1\n")))
(check-equal? (errors "Profile: P\nParent: Observation\n* category 1..5\n* value[x] 1..1\n* component 2..*\n") '())
;; `contains` on an element nothing says how to slice
(check-regexp-match #rx"cannot create the slice `systolic`: `component` is not sliced"
                    (car (errors "Profile: P\nParent: Observation\n* component contains systolic 1..1\n")))
(check-equal? (errors "Profile: P\nParent: Observation\n* component ^slicing.discriminator[0].type = #pattern\n* component ^slicing.discriminator[0].path = \"code\"\n* component ^slicing.rules = #open\n* component contains systolic 1..1\n") '())
;; an extension slice with no Extension behind it, outside an Extension
(check-regexp-match #rx"`foo` names no Extension" (car (errors "Profile: P\nParent: Observation\n* extension contains foo 0..1\n")))
(check-equal? (errors "Extension: E\n* extension contains foo 0..1\n") '())
;; an invariant no Invariant defines
(check-regexp-match #rx"invariant `obs-x` not found" (car (errors "Profile: P\nParent: Observation\n* obeys obs-x\n")))
(check-equal? (errors "Invariant: obs-x\nDescription: \"d\"\nSeverity: #error\n\nProfile: P\nParent: Observation\n* obeys obs-x\n") '())
;; a Parent nothing defines, for an Extension and for a Logical model as for a Profile
(check-regexp-match #rx"Parent `ParentExtension` not found" (car (errors "Extension: E\nParent: ParentExtension\n")))
(check-regexp-match #rx"Parent `BaseModel` not found" (car (errors "Logical: M\nParent: BaseModel\n")))
(check-equal? (errors "Logical: B\n* x 0..1 string \"x\"\n\nLogical: M\nParent: B\n") '())

(delete-directory/files work)
