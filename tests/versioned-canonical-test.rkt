#lang racket/base

;;; versioned-canonical-test.rkt — a type or a profile may be named by its canonical, pinned or not.
;;;
;;; Found asking a model to state a published profile's requirements in FSH: told the profile is "a
;;; constraint on http://hl7.org/fhir/StructureDefinition/Condition|4.0.1", it wrote that as its
;;; Parent, as SUSHI accepts — and the compiler took the canonical for a type NAME: the profile's
;;; `type` was the whole url, every element id was rooted at it, an Instance of it was "not found",
;;; and nothing said why. The same for `only <canonical>|<version>`, for an alias written with a
;;; pin (`$ext|5.3.0`), and for a type name nothing defines, which became the element's type code.

(require rackunit racket/file racket/list
         (only-in fsh/check check-fsh-project check-report-diagnostics check-report-artifacts)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line))

(define root (make-temporary-directory "fsh-versioned-~a"))
(with-output-to-file (build-path root "sushi-config.yaml")
  (lambda () (display "canonical: http://example.org/t\nfhirVersion: 4.0.1\nid: t\nname: T\nversion: 0.1.0\nstatus: draft\n")))
(define (write! text) (with-output-to-file (build-path root "p.fsh") #:exists 'truncate (lambda () (display text))))
(define (errors r) (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics r)))
(define (sd r id) (findf (lambda (a) (equal? (hash-ref a 'id #f) id)) (check-report-artifacts r)))
(define (element s id) (findf (lambda (e) (equal? (hash-ref e 'id) id)) (hash-ref (hash-ref s 'differential) 'element)))

;; a Parent by its core canonical, pinned: the type, the element ids, and an Instance of the profile
(write! (string-append
         "Alias: $vital = http://hl7.org/fhir/StructureDefinition/vitalsigns\n\n"
         "Profile: P1\nParent: http://hl7.org/fhir/StructureDefinition/Condition|4.0.1\nId: p1\n"
         "* code 1..1 MS\n"
         "* code only http://hl7.org/fhir/StructureDefinition/CodeableConcept|4.0.1\n"
         "* subject only Reference(http://hl7.org/fhir/StructureDefinition/Patient|4.0.1)\n\n"
         "Profile: P2\nParent: http://hl7.org/fhir/StructureDefinition/Observation\nId: p2\n"
         "* hasMember only Reference($vital|4.0.1)\n"
         "* value[x] only Quantity\n\n"
         "Profile: P3\nParent: P1\nId: p3\n* severity MS\n\n"
         "Instance: c1\nInstanceOf: P1\n* code = http://snomed.info/sct#1\n* subject = Reference(Patient/x)\n\n"
         "Instance: c3\nInstanceOf: P3\n* code = http://snomed.info/sct#1\n* subject = Reference(Patient/x)\n"))
(define r (check-fsh-project root))
(check-equal? (map diagnostic-message (errors r)) '())
(check-equal? (hash-ref (sd r "p1") 'type) "Condition")
(check-equal? (hash-ref (sd r "p1") 'baseDefinition) "http://hl7.org/fhir/StructureDefinition/Condition")
(check-equal? (map (lambda (e) (hash-ref e 'id)) (hash-ref (hash-ref (sd r "p1") 'differential) 'element))
              '("Condition.code" "Condition.subject"))
;; `only` a core type by its pinned canonical names the type and narrows nothing
(check-false (hash-ref (element (sd r "p1") "Condition.code") 'type #f))
(check-equal? (hash-ref (sd r "p2") 'type) "Observation")
;; an alias written with a pin: the profile it names, the pin kept on the canonical
(check-equal? (hash-ref (car (hash-ref (element (sd r "p2") "Observation.hasMember") 'type)) 'targetProfile)
              '("http://hl7.org/fhir/StructureDefinition/vitalsigns|4.0.1"))
;; a profile of the profile, and Instances of both
(check-equal? (hash-ref (sd r "p3") 'type) "Condition")
(check-equal? (sort (for/list ([a (check-report-artifacts r)] #:when (equal? (hash-ref a 'resourceType #f) "Condition")) (hash-ref a 'id)) string<?)
              '("c1" "c3"))

;; a profile in `only`, by pinned canonical and by pinned alias: its base type as the code, the pin
;; on the profile (the code was the canonical's last step, pin and all)
(write! (string-append
         "Alias: $vital = http://hl7.org/fhir/StructureDefinition/vitalsigns\n\n"
         "Profile: P4\nParent: Observation\nId: p4\n"
         "* hasMember only Reference(Observation)\n"
         "* contained only http://hl7.org/fhir/StructureDefinition/vitalsigns|4.0.1\n\n"
         "Profile: P5\nParent: Observation\nId: p5\n"
         "* contained only $vital|4.0.1\n"))
(define r2 (check-fsh-project root))
(check-equal? (map diagnostic-message (errors r2)) '())
(for ([id '("p4" "p5")])
  (check-equal? (hash-ref (element (sd r2 id) "Observation.contained") 'type)
                (list (hasheq 'code "Observation" 'profile '("http://hl7.org/fhir/StructureDefinition/vitalsigns|4.0.1")))
                id))

;; a type nothing defines is an error at its rule, with the nearest name; a url is not judged
(write! (string-append
         "Profile: P6\nParent: Observation\nId: p6\n"
         "* value[x] only Quantty\n"
         "* subject only Reference(Patient)\n"
         "* contained only http://example.org/elsewhere/StructureDefinition/unknown\n"))
(define es (errors (check-fsh-project root)))
(check-equal? (length es) 1)
(check-regexp-match #rx"type `Quantty` not found: .*[(]did you mean `Quantity`[?][)]" (diagnostic-message (car es)))
(check-equal? (loc-line (diagnostic-loc (car es))) 4)

(delete-directory/files root)
