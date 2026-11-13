#lang racket/base

;;; primitive-types-test.rkt — `only dateTime` names a TYPE, whatever else the program has loaded.
;;;
;;; FHIR's primitive types are registered by fhir/primitives/primitives, not by a tower. The compiler
;;; did not load it, so in a program that had loaded only fsh/compile and a tower `dateTime` was not
;;; a type and `* effective[x] only dateTime` was written as a profile:
;;;   {"code": "dateTime", "profile": ["http://hl7.org/fhir/StructureDefinition/dateTime"]}
;;; `raco fsh` got it right by accident (compiling a project loaded fml, which loads fhirpath, which
;;; loads the primitives); `fsh->fhir` alone and the corpus tests did not. The compile runs in a
;;; process of its own: in this one another test may already have loaded the primitives.

(require rackunit racket/port racket/system)

(define program
  (string-append
   "(require (only-in fhir/r4/schemas) fsh/compile)"
   "(define sd (car (fsh->fhir \"Profile: P\\nParent: Observation\\n* effective[x] only dateTime or Period\\n* value[x] only string\\n\")))"
   "(for ([e (hash-ref (hash-ref sd 'differential) 'element)]) (write (list (hash-ref e 'id) (hash-ref e 'type))) (newline))"))

(define out (open-output-string))
(define err (open-output-string))
(define code
  (parameterize ([current-output-port out] [current-error-port err])
    (system*/exit-code (find-executable-path (find-system-path 'exec-file)) "-l" "racket/base" "-e" program)))

(check-equal? code 0 (get-output-string err))
(check-equal? (with-input-from-string (get-output-string out) (lambda () (list (read) (read))))
              '(("Observation.effective[x]" (#hasheq((code . "dateTime")) #hasheq((code . "Period"))))
                ("Observation.value[x]" (#hasheq((code . "string"))))))
