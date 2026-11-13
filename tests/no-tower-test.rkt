#lang racket/base

;;; `fsh->fhir` from a program that has loaded no FHIR version: it loads the one `fsh-fhir-version`
;;; names. With no tower an Instance of a profile in the same text was "InstanceOf `LabResult` not
;;; found". This file must require NO tower.

(require rackunit (only-in fsh/compile fsh->fhir))

(define made
  (fsh->fhir (string-append "Profile: LabResult\nParent: Observation\n* status = #final\n\n"
                            "Instance: a1\nInstanceOf: LabResult\n* status = #final\n* code = http://loinc.org#4548-4\n")))
(check-equal? (map (lambda (r) (hash-ref r 'resourceType)) made) '("StructureDefinition" "Observation"))
(check-equal? (hash-ref (hash-ref (cadr made) 'meta) 'profile) '("http://example.org/StructureDefinition/LabResult"))
