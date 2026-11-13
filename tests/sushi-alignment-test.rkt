#lang racket/base

;;; sushi-alignment-test.rkt — what the published guides showed SUSHI does and this compiler did
;;; not (2026-11-13). Each case is a small FSH text and the JSON SUSHI writes for it; the corpus tests
;;; (tests/ig-*-test.rkt) hold the same behaviours on the guides themselves, with floors.
;;;
;;; The differential holds what CHANGES, in the base definition's order:
;;;   * a cardinality or type equal to the definition an element starts from is not written, also
;;;     below a datatype (`name.family 0..1`) and inside a slice (`component[x].value[x] 0..1`);
;;;   * `extension contains …` on a datatype's extension adds no slicing (FHIR slices it already);
;;;   * a type-specific choice path addresses the choice itself once the Parent narrowed it to one type;
;;;   * an element's children follow its datatype's own order (`telecom.extension` before
;;;     `telecom.system`), and the Parent's slices the order its snapshot holds them in.
;;; An instance:
;;;   * an object assigned where an object stands is laid over it (`coding.version`, then `coding =`);
;;;   * an element the instance writes holds the pattern its profile pins there, required or not;
;;;   * `Title:` / `Description:` fill title / description of a `Usage: #definition` instance only;
;;;   * an instance named like a number can be embedded by that name.

(require rackunit racket/list
         (only-in fhir/r4/schemas)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fsh/compile fsh->fhir canonical-base fsh-fhir-version))

(define r4-core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))

;; compiled : string ... -> (listof jsexpr)   the FSH lines compiled for R4 against the core package
(define (compiled . lines)
  (parameterize ([current-sd-packages (list r4-core)] [fsh-fhir-version "4.0.1"] [canonical-base "http://example.org"])
    (fsh->fhir (apply string-append (map (lambda (l) (string-append l "\n")) lines)))))
(define (elements sd) (hash-ref (hash-ref sd 'differential) 'element))
(define (ids sd) (map (lambda (e) (hash-ref e 'id)) (elements sd)))
;; element : sd id -> hasheq | #f   the differential element of that id (as a hasheq, to compare)
(define (element sd id)
  (define e (findf (lambda (e) (equal? (hash-ref e 'id) id)) (elements sd)))
  (and e (for/hasheq ([(k v) (in-hash e)]) (values k v))))
(define (named rs id) (findf (lambda (r) (equal? (hash-ref r 'id #f) id)) rs))

(cond
  [(not r4-core) (printf "sushi-alignment: skipped (needs the fhir-spec package)\n")]
  [else
   (test-case "what an element already is, is not written: below a datatype, inside a slice"
     (define sd (car (compiled "Profile: P" "Parent: Patient"
                               "* name.family 0..1 MS"
                               "* name.given 0..* MS"
                               "* address.line 1..* MS")))
     (check-equal? (element sd "Patient.name.family") (hasheq 'id "Patient.name.family" 'path "Patient.name.family" 'mustSupport #t))
     (check-equal? (element sd "Patient.name.given") (hasheq 'id "Patient.name.given" 'path "Patient.name.given" 'mustSupport #t))
     (check-equal? (element sd "Patient.address.line")
                   (hasheq 'id "Patient.address.line" 'path "Patient.address.line" 'min 1 'mustSupport #t))
     (define obs (car (compiled "Profile: O" "Parent: Observation"
                                "* component ^slicing.discriminator.type = #pattern"
                                "* component ^slicing.discriminator.path = \"code\""
                                "* component ^slicing.rules = #open"
                                "* component contains a 0..1"
                                "* component[a].value[x] 1..1")))
     ;; the slice itself is new: its min and max are written; its child keeps what changed (min)
     (check-equal? (element obs "Observation.component:a")
                   (hasheq 'id "Observation.component:a" 'path "Observation.component" 'sliceName "a" 'min 0 'max "1"))
     (check-equal? (element obs "Observation.component:a.value[x]")
                   (hasheq 'id "Observation.component:a.value[x]" 'path "Observation.component.value[x]" 'min 1)))

   (test-case "`contains` on a datatype's extension writes no slicing; on a resource's it does"
     (define c (cadr (compiled "Extension: Side" "Id: side" "* value[x] only code"
                               ""
                               "Profile: C" "Parent: Condition"
                               "* bodySite.extension contains Side named side 0..1"
                               "* extension contains Side named rootSide 0..1")))
     (check-false (element c "Condition.bodySite.extension"))
     (check-not-false (element c "Condition.bodySite.extension:side"))
     (check-equal? (hash-ref (element c "Condition.extension") 'slicing)
                   (hasheq 'discriminator (list (hasheq 'type "value" 'path "url")) 'ordered #f 'rules "open")))

   (test-case "a choice the Parent narrowed to one type is addressed by its typed name"
     (define rs (compiled "Profile: Dated" "Parent: DiagnosticReport"
                          "* effective[x] only dateTime"
                          ""
                          "Profile: Flagged" "Parent: Dated"
                          "* effective[x] and effectiveDateTime MS"))
     (check-equal? (ids (named rs "Flagged")) '("DiagnosticReport.effective[x]"))
     (check-equal? (element (named rs "Flagged") "DiagnosticReport.effective[x]")
                   (hasheq 'id "DiagnosticReport.effective[x]" 'path "DiagnosticReport.effective[x]" 'mustSupport #t))
     ;; not narrowed: the type slice, as before
     (define open (car (compiled "Profile: Open" "Parent: DiagnosticReport" "* effectiveDateTime MS")))
     (check-equal? (ids open) '("DiagnosticReport.effective[x]" "DiagnosticReport.effective[x]:effectiveDateTime")))

   (test-case "children in their datatype's order; the Parent's slices in its order"
     (define l (cadr (compiled "Extension: Via" "Id: via" "* value[x] only boolean"
                               ""
                               "Profile: L" "Parent: Location"
                               "* telecom.value MS"
                               "* telecom.system MS"
                               "* telecom.extension contains Via named via 0..1")))
     (check-equal? (ids l) '("Location.telecom.extension:via" "Location.telecom.system" "Location.telecom.value"))
     (define rs (compiled "Profile: Panel" "Parent: Observation"
                          "* component ^slicing.discriminator.type = #pattern"
                          "* component ^slicing.discriminator.path = \"code\""
                          "* component ^slicing.rules = #open"
                          "* component contains first 0..1 and second 0..1 and third 0..1"
                          ""
                          "Profile: Derived" "Parent: Panel"
                          "* component[third] MS"
                          "* component[first] MS"
                          "* component contains fourth 0..1"
                          "* component[second] MS"))
     (check-equal? (ids (named rs "Derived"))
                   '("Observation.component:first" "Observation.component:second" "Observation.component:third"
                     "Observation.component:fourth")))

   (test-case "an object assigned over an object keeps the keys it does not set"
     (define p (car (compiled "Instance: p" "InstanceOf: Practitioner"
                              "* qualification.code.coding.version = \"2.7\""
                              "* qualification.code.coding = http://terminology.hl7.org/CodeSystem/v2-0360#MD \"Doctor of Medicine\"")))
     (check-equal? (car (hash-ref (hash-ref (car (hash-ref p 'qualification)) 'code) 'coding))
                   (hasheq 'version "2.7" 'system "http://terminology.hl7.org/CodeSystem/v2-0360" 'code "MD" 'display "Doctor of Medicine"))
     ;; a second value for the same keys replaces the first
     (define o (car (compiled "Instance: o" "InstanceOf: Observation"
                              "* status = #final" "* code.text = \"x\""
                              "* subject = Reference(Patient/a)" "* subject = Reference(Patient/b)")))
     (check-equal? (hash-ref o 'subject) (hasheq 'reference "Patient/b")))

   (test-case "an element the instance writes holds its profile's pattern, required or not"
     (define rs (compiled "Profile: Burden" "Parent: Observation"
                          "* valueQuantity = 'mg' \"mg\""
                          ""
                          "Instance: b1" "InstanceOf: Burden"
                          "* status = #final" "* code.text = \"burden\""
                          "* valueQuantity.value = 57.1"
                          ""
                          "Instance: b2" "InstanceOf: Burden"
                          "* status = #final" "* code.text = \"burden\""))
     (check-equal? (hash-ref (named rs "b1") 'valueQuantity)
                   (hasheq 'value 57.1 'unit "mg" 'system "http://unitsofmeasure.org" 'code "mg"))
     ;; an optional element the instance does not write stays out
     (check-false (hash-has-key? (named rs "b2") 'valueQuantity)))

   (test-case "Title and Description fill a definition's elements, not an example's"
     (define rs (compiled "Instance: clinic" "InstanceOf: Location" "Usage: #example"
                          "Title: \"The clinic\"" "Description: \"An example location\""
                          "* name = \"Clinic\""
                          ""
                          "Instance: op" "InstanceOf: OperationDefinition" "Usage: #definition"
                          "Title: \"An operation\"" "Description: \"What it does\""
                          "* name = \"Op\"" "* status = #draft" "* kind = #operation" "* code = #op"
                          "* system = true" "* type = false" "* instance = false"))
     (check-false (hash-has-key? (named rs "clinic") 'description))
     (check-equal? (hash-ref (named rs "op") 'title) "An operation")
     (check-equal? (hash-ref (named rs "op") 'description) "What it does"))

   (test-case "an instance named like a number is embedded by that name"
     (define rs (compiled "Instance: 39252" "InstanceOf: Procedure" "Usage: #inline"
                          "* status = #completed" "* subject = Reference(Patient/a)"
                          ""
                          "Instance: b" "InstanceOf: Bundle"
                          "* type = #collection"
                          "* entry[+].resource = 39252"))
     (check-equal? (hash-ref (hash-ref (car (hash-ref (named rs "b") 'entry)) 'resource) 'resourceType) "Procedure"))])
