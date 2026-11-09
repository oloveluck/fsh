#lang racket/base

;;; guide-test.rkt — guide.md (the language guide agents read: fhir-mcp serves it as learn://fsh, the
;;; fsh skill points to it) keeps its promises: every ```fsh block is a project that checks with no
;;; error (compiled, and every example validated against its profile, as `raco fsh check` and the
;;; workspace do), and each problem the text quotes is what the checker reports.

(require rackunit racket/runtime-path racket/file racket/list racket/string
         (only-in fsh/check check-fsh-project check-report-diagnostics check-report-examples)
         (only-in fsh/workspace guide)
         (only-in fhir/diagnostic diagnostic-severity diagnostic-message diagnostic-loc loc-line))

(define-runtime-path guide-file "../guide.md")
(define text (file->string guide-file))
(define blocks (map cadr (regexp-match* #px"```fsh\n(.*?)```" text #:match-select values)))

;; check-files : (listof (cons name text)) -> check-report   one project in a fresh directory
(define (check-files files)
  (define dir (make-temporary-directory))
  (for ([f (in-list files)]) (call-with-output-file (build-path dir (car f)) (lambda (o) (write-string (cdr f) o))))
  (begin0 (check-fsh-project dir #:fhir-version "4.0.1")
          (delete-directory/files dir)))
(define (errors r) (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-report-diagnostics r)))
(define (messages r) (map diagnostic-message (errors r)))

(test-case "the guide the workspace serves is this file"
  (check-equal? guide text))

(test-case "every fsh block checks clean, examples validated"
  (check-true (>= (length blocks) 2))
  (for ([b (in-list blocks)] [i (in-naturals)])
    (define r (check-files (list (cons (format "block~a.fsh" i) b))))
    (check-equal? (messages r) '() b)
    (check-true (>= (check-report-examples r) 1) "each block has an example to validate")))

(define worked (for/first ([b (in-list blocks)] #:when (regexp-match? #rx"HbA1cObservation" b)) b))

(test-case "the worked example's failure reads as the guide says"
  (define r (check-files (list (cons "a.fsh" (regexp-replace #rx"(Instance: hba1c-example\nInstanceOf: HbA1cObservation\n)[*] status = #final"
                                                              worked "\\1* status = #preliminary")))))
  (check-equal? (length (errors r)) 1)
  (check-regexp-match #rx"Observation.status: value does not match required pattern \"final\" [(]constrained by profile HbA1cObservation" (car (messages r))))

(test-case "each common problem is reported as the guide quotes it"
  (define (one files rx)
    (define ms (messages (check-files files)))
    (check-true (ormap (lambda (m) (regexp-match? rx m)) ms) (format "~s not in ~s" rx ms)))
  (one (list (cons "a.fsh" "Profile: MyPatient\nParent: Patient\n* nmae 1..1\n")) #rx"`nmae` is not an element of Patient")
  (one (list (cons "a.fsh" "Profile: X\nParent: NoSuchProfile\n* status MS\n")) #rx"Parent `NoSuchProfile` not found")
  (one (list (cons "a.fsh" "ValueSet: A\nId: same\n* http://loinc.org#1\n") (cons "b.fsh" "ValueSet: B\nId: same\n* http://loinc.org#2\n"))
       #rx"the ValueSet id `same` is already used")
  (one (list (cons "a.fsh" (regexp-replace* #rx"\n[*] component\\[[01]\\][^\n]*" (car blocks) "")))
       #rx"minimum required = 2, but found 0")
  (one (list (cons "a.fsh" "Profile: P\nParent: Patient\n* insert NoSuchRuleSet\n")) #rx"RuleSet `NoSuchRuleSet` not found")
  (one (list (cons "a.fsh" "Profile: P  # a comment\nParent: Patient\n")) #rx"unexpected sequence `#`")
  ;; a choice type a slice excludes: at the instance rule of that slice, naming the slice's `only`
  (define bp (string-append
              "Alias: $loinc = http://loinc.org\n\nProfile: BP\nParent: Observation\n"
              "* component ^slicing.discriminator.type = #pattern\n* component ^slicing.discriminator.path = \"code\"\n"
              "* component ^slicing.rules = #open\n* component contains systolic 1..1 and diastolic 1..1\n"
              "* component[systolic].code = $loinc#8480-6\n* component[systolic].value[x] only Quantity\n"
              "* component[diastolic].code = $loinc#8462-4\n* component[diastolic].value[x] only Quantity\n\n"
              "Instance: bp-1\nInstanceOf: BP\nUsage: #example\n* status = #final\n* code = $loinc#85354-9\n"
              "* component[systolic].valueQuantity = 150 'mm[Hg]'\n* component[diastolic].valueString = \"eighty\"\n"))
  (define slice-errors (errors (check-files (list (cons "a.fsh" bp)))))
  (check-equal? (length slice-errors) 1 (format "~s" (map diagnostic-message slice-errors)))
  (check-regexp-match #rx"valueString is not allowed in slice 'diastolic' [(]value\\[x\\]: Quantity[)] [(]constrained by profile BP at line 12[)]"
                      (diagnostic-message (car slice-errors)))
  (check-equal? (loc-line (diagnostic-loc (car slice-errors))) 20))
