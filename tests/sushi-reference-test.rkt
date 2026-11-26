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

;; ---- second pass (fhir-examples' protocol, "H part 3, second pass") ------------------------------
;; strings: `\uXXXX` (a surrogate pair is one character) in quoted strings; `\n \r \t \uXXXX` in a
;; triple-quoted one, line by line before its indentation is trimmed (SUSHI's importer)
(let ([sd (car (resources "Profile: P\nParent: Observation\nTitle: \"a \\u270E b \\uD83C\\uDC31 \\\\u2744\"\nDescription: \"\"\"\n  one \\n two \\t \\u2603\n  \"\"\"\n"))])
  (check-equal? (hash-ref sd 'title) "a ✎ b 🀱 \\u2744")
  (check-equal? (hash-ref sd 'description) "one \n two \t ☃"))
;; the rules under `* a and b MS` take the LAST path as their context
(let ([sd (car (resources "Profile: P\nParent: Patient\n* name and birthDate MS\n  * ^short = \"foo\"\n"))])
  (check-equal? (hash-ref (element sd "Patient.birthDate") 'short) "foo")
  (check-false (hash-has-key? (element sd "Patient.name") 'short)))
;; an id that is a FHIR name but no FHIR id is written with `-` for `_`, with a warning
(check-equal? (hash-ref (car (resources "ValueSet: SimpleVS\nId: SimpleVS_ID\n")) 'id) "SimpleVS-ID")
(check-equal? (hash-ref (car (resources "ValueSet: Simple_VS\n")) 'url) "http://example.org/ValueSet/Simple-VS")
;; an added element of several types: `or` is no part of its short (it was the short, and the short
;; was taken for the definition); a choice element ends in `[x]`; several Reference targets are none
(let ([sd (car (resources "Resource: T\n* isValid[x] 1..1 boolean or number \"short boolean\"\n"))])
  (check-equal? (hash-ref (element sd "T.isValid[x]") 'short) "short boolean"))
(check-regexp-match #rx"several types, so it is a choice element: name it `isValid\\[x\\]`"
                    (car (errors "Resource: T\n* isValid 1..1 boolean or number \"s\"\n")))
(check-equal? (errors "Resource: T\n* who 0..1 Reference(Patient or Group) \"s\"\n") '())
;; MustSupport belongs to profiles and extensions; one standards status an element
(check-regexp-match #rx"MustSupport \\(MS\\) is not permitted in a logical model" (car (errors "Logical: L\n* a 0..1 MS string \"s\"\n")))
(check-regexp-match #rx"one standards status" (car (errors "Profile: P\nParent: Patient\n* name TU N\n")))
;; an extension has a value or sub-extensions, not both
(let ([ext "Extension: E\n* extension contains channel 1..1\n* extension[channel].value[x] only code\n"])
  (check-equal? (errors (string-append ext "* value[x] 0..0\n")) '())
  (check-regexp-match #rx"a value or sub-extensions, not both" (car (errors (string-append ext "* value[x] 1..1\n")))))
;; a value set of excludes alone; a caret on a concept a value set or code system does not list
(check-regexp-match #rx"excludes codes but includes none" (car (errors "ValueSet: V\n* exclude http://x.org#a\n")))
(let ([vs (car (resources "Alias: ZOO = http://zoo.org\nValueSet: V\n* ZOO#hippo \"Hippo\"\n* ZOO#hippo ^designation.value = \"hipo\"\n"))])
  (check-equal? (hash-ref (car (hash-ref (car (hash-ref (car (hash-ref (hash-ref vs 'compose) 'include)) 'concept)) 'designation)) 'value) "hipo"))
(check-regexp-match #rx"not listed by value set" (car (errors "Alias: ZOO = http://zoo.org\nValueSet: V\n* ZOO#hippo\n* ZOO#lion ^designation.value = \"x\"\n")))
(check-regexp-match #rx"is not in the code system" (car (errors "CodeSystem: C\n* #bear\n* #anteater ^designation.value = \"x\"\n")))
(check-regexp-match #rx"cannot be the parent of `sunbear`" (car (errors "CodeSystem: C\n* #bear #sunbear\n")))
;; a profile's path: no index, a choice's own types, a slice that is declared
(check-regexp-match #rx"an index belongs to an instance" (car (errors "Profile: P\nParent: Observation\n* category[0].text = \"x\"\n")))
(check-regexp-match #rx"value\\[x\\]` takes no Decimal" (car (errors "Profile: P\nParent: Observation\n* valueDecimal = 1.23\n")))
(check-regexp-match #rx"no slice `systolic` of `component`" (car (errors "Profile: P\nParent: Observation\n* component[systolic].code = http://loinc.org#8480-6\n")))
;; a base element below a primitive's id: what does not change is not written (`max` was)
(let ([sd (car (resources "Profile: P\nParent: Patient\n* name.family.id.id 1..1\n"))])
  (check-false (hash-has-key? (element sd "Patient.name.family.id.id") 'max)))
;; a Mapping with no Source, a RuleSet inserted under a concept that nothing defines
(check-regexp-match #rx"has no Source" (car (errors "Mapping: M\n")))
(check-regexp-match #rx"RuleSet `R` not found" (car (errors "CodeSystem: C\n* #a\n* #a insert R\n")))
;; an instance without an element its InstanceOf requires, or with a value its profile contradicts:
;; errors at the instance, which is still written (SUSHI: validateRequiredElements,
;; ValueAlreadyAssignedError); a JSON `false` is a value
(check-regexp-match #rx"`Questionnaire.status` is required [(]1 at least[)] and the instance gives it 0 times: add `[*] status = …`"
                    (car (errors "Instance: q\nInstanceOf: Questionnaire\n* title = \"x\"\n")))
(check-regexp-match #rx"`Observation.component.code` is required"
                    (car (errors "Instance: o\nInstanceOf: Observation\n* status = #final\n* code = http://loinc.org#1-8\n* component[0].valueString = \"x\"\n")))
(check-equal? (errors "Instance: p\nInstanceOf: Patient\n") '())
(check-equal? (errors "Instance: od\nInstanceOf: OperationDefinition\n* name = \"x\"\n* status = #draft\n* kind = #operation\n* code = #x\n* system = false\n* type = false\n* instance = true\n") '())
(let ([p "Profile: P\nParent: Observation\n* status = #final\n* code = http://loinc.org#85354-9\n\nInstance: x\nInstanceOf: P\n* subject = Reference(Patient/a)\n"])
  (check-equal? (errors (string-append p "* status = #final\n* code = http://loinc.org#85354-9\n")) '())
  (check-equal? (errors (string-append p "* status = #final\n* code.coding[1] = http://snomed.info/sct#1\n* code.coding[0] = http://loinc.org#85354-9\n")) '())
  (check-equal? (length (errors (string-append p "* status = #amended\n* code = http://loinc.org#1234-5\n"))) 2)
  (check-regexp-match #rx"gives `Observation.status` the value \"amended\", and its profile assigns \"final\" there"
                      (car (errors (string-append p "* status = #amended\n")))))
;; third pass: what the second left named
;; a value its element cannot hold, in a profile, a caret or an instance (an instance's is said at
;; it, the value kept); an empty text and `12E4` into an integer are taken, as SUSHI takes them
(check-regexp-match #rx"Cannot assign \"12:30:01.999-05:00\": it is no time" (car (errors "Profile: P\nParent: Observation\n* valueTime = 12:30:01.999-05:00\n")))
(check-regexp-match #rx"Cannot assign the number 64: it is no Address" (car (errors "Profile: P\nParent: Observation\n* ^extension[0].valueAddress = 0064\n")))
(check-regexp-match #rx"Cannot assign 0.45: it is no integer" (car (errors "CodeSystem: Z\n* #a \"A\"\n* #a ^extension[0].valueInteger = 0.4500\n")))
(check-regexp-match #rx"Cannot assign \"changed\": it is no date" (car (errors "Instance: p\nInstanceOf: Patient\n* birthDate = \"changed\"\n")))
(check-equal? (errors "Profile: P\nParent: Observation\n* valueInteger = 12E4\n* ^comment = \"\"\n") '())
;; `^contained` naming an instance nothing defines; a resource named by one that is
(check-regexp-match #rx"Cannot assign `myResource`: it is no resource [(]no instance of that name is defined[)]"
                    (car (errors "Profile: P\nParent: Observation\n* ^contained = myResource\n")))
;; an `only` type the base element does not take; one built on a type it takes
(check-regexp-match #rx"the type `Coding` does not match any of the allowed types of `value\\[x\\]`"
                    (car (errors "Profile: P\nParent: Observation\n* value[x] only Coding\n")))
(check-regexp-match #rx"the type `CodeableReference[(]Practitioner[)]` does not match any of the allowed types of `performer`: Reference"
                    (car (errors "Profile: P\nParent: Observation\n* performer only CodeableReference(Practitioner)\n")))
(check-equal? (errors "Profile: P\nParent: Observation\n* value[x] only Quantity or SimpleQuantity\n* contained only Patient\n") '())
;; an instance list longer than its profile allows (SUSHI: no element at `component[1]`)
(check-regexp-match #rx"`Observation.component` allows 1, and the instance gives it 2"
                    (car (errors (string-append "Profile: P\nParent: Observation\n* component 1..1\n\nInstance: o\nInstanceOf: P\n"
                                                "* status = #final\n* code = http://loinc.org#1-8\n"
                                                "* component[0].code = http://loinc.org#2-6\n* component[1].code = http://loinc.org#3-4\n"))))
;; a logical model's element under a parent it never added; a caret on a concept a code system
;; with no concepts lacks; a slicing without its parts; an Extension's slice it never declared
(check-regexp-match #rx"no element `35` to add `35.79` under" (car (errors "Logical: L\n* 35.79 1..1 boolean \"s\"\n")))
(check-equal? (errors "Logical: L\n* 35 0..* BackboneElement \"s\"\n* 35.79 1..1 boolean \"s\"\n") '())
(check-regexp-match #rx"is not in the code system" (car (errors "CodeSystem: Z\n* #a ^property[0].valueString = \"x\"\n")))
(check-regexp-match #rx"Observation.component \\^slicing.rules: Missing required value"
                    (car (errors "Profile: P\nParent: Observation\n* component ^slicing.discriminator[0].type = #pattern\n* component ^slicing.discriminator[0].path = \"code\"\n")))
(check-regexp-match #rx"discriminator\\[0\\].type: Invalid value: #changed"
                    (car (errors "Profile: P\nParent: Observation\n* component ^slicing.discriminator[0].type = #changed\n* component ^slicing.discriminator[0].path = \"code\"\n* component ^slicing.rules = #open\n")))
(check-regexp-match #rx"no slice `channel` of `extension`" (car (errors "Extension: E\n* extension[channel].value[x] only code\n* value[x] 0..0\n")))
(check-equal? (errors "Profile: P\nParent: Patient\n* extension contains http://hl7.org/fhir/StructureDefinition/patient-birthPlace named bp 0..1\n* extension[bp].valueAddress.city = \"x\"\n") '())
;; a sub-extension that says nothing of its value: its url alone (no `extension 0..0`), and a
;; list's unset entries are null, not false
(let ([sd (car (resources "Extension: E\n* extension contains a 0..1 and b 0..1\n* extension[a].value[x] only code\n* value[x] 0..0\n"))])
  (check-not-false (element sd "Extension.extension:a.extension"))
  (check-false (element sd "Extension.extension:b.extension")))
(let ([cs (car (resources "CodeSystem: Z\n* #a \"A\"\n* #a ^extension[1].valueBoolean = true\n"))])
  (check-equal? (car (hash-ref (car (hash-ref cs 'concept)) 'extension)) 'null))

(delete-directory/files work)
