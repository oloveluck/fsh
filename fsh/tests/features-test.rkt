#lang racket/base

;;; features-test.rkt — FSH features without a SUSHI golden pair (parameterized RuleSets, snapshot).

(require rackunit
         racket/list
         (only-in fhir/r5/schemas)
         (only-in fhir/fsh/compile fsh->fhir canonical-base emit-snapshot?)
         (only-in fhir/fsh/decompile fhir->fsh))

(define (only r) (car r))
(define (elements sd) (hash-ref (hash-ref sd 'differential) 'element))
(define (by-path sd p) (findf (lambda (e) (equal? (hash-ref e 'path) p)) (elements sd)))

(parameterize ([canonical-base "http://example.org"])
  ;; ---- parameterized RuleSet: {param} substitution + insert expansion --------------------
  (define sd
    (only (fsh->fhir (string-append
                      "RuleSet: Req(path, card)\n"
                      "* {path} {card} MS\n\n"
                      "Profile: P\nParent: Patient\nId: p\n"
                      "* insert Req(name, 1..*)\n"
                      "* insert Req(gender, 1..1)\n"))))
  (define name-el (by-path sd "Patient.name"))
  (define gender-el (by-path sd "Patient.gender"))
  (check-equal? (hash-ref name-el 'min) 1)
  (check-equal? (hash-ref name-el 'max) "*")
  (check-equal? (hash-ref name-el 'mustSupport) #t)
  (check-equal? (hash-ref gender-el 'max) "1")
  (check-equal? (hash-ref gender-el 'mustSupport) #t))

;; ---- Mapping: injects StructureDefinition.mapping + ElementDefinition.mapping into the Source ----
(parameterize ([canonical-base "http://example.org"])
  (define sds (fsh->fhir (string-append
                          "Profile: MyPat\nParent: Patient\nId: mypat\n* name 1..*\n\n"
                          "Mapping: M\nId: m\nSource: MyPat\nTarget: \"HL7 V2\"\nTitle: \"V2 Map\"\n"
                          "* identifier -> \"PID-3\"\n* name -> \"PID-5\" \"the name\"\n")))
  (define sd (only sds))                                ; the Mapping does not produce its own artifact
  (check-equal? (length sds) 1)
  (check-equal? (hash-ref sd 'mapping) (list (hasheq 'identity "m" 'name "V2 Map" 'uri "HL7 V2")))
  (define name-el (by-path sd "Patient.name"))
  (check-equal? (hash-ref name-el 'mapping) (list (hasheq 'identity "m" 'map "PID-5" 'comment "the name")))
  ;; a mapped element not otherwise constrained is added
  (check-true (and (by-path sd "Patient.identifier") #t)))

;; ---- reslicing + multi-discriminator slicing (compile + round-trip) --------------------------
(parameterize ([canonical-base "http://example.org"])
  (define (ids sd) (map (lambda (e) (hash-ref e 'id)) (elements sd)))
  ;; multi-discriminator: two ^slicing.discriminator[i] entries survive
  (define md (only (fsh->fhir (string-append
                               "Profile: MD\nParent: Observation\nId: md\n"
                               "* component ^slicing.discriminator[0].type = #pattern\n"
                               "* component ^slicing.discriminator[0].path = \"code\"\n"
                               "* component ^slicing.discriminator[1].type = #value\n"
                               "* component ^slicing.discriminator[1].path = \"value\"\n"
                               "* component ^slicing.rules = #open\n* component contains a 1..1\n"))))
  (define disc (hash-ref (hash-ref (by-path md "Observation.component") 'slicing) 'discriminator))
  (check-equal? (length disc) 2)
  (check-equal? (fsh->fhir (fhir->fsh md)) (list md))          ; round-trips

  ;; reslicing: a slice of a slice gets id …:Lab/Chem and round-trips
  (define rs (only (fsh->fhir (string-append
                               "Profile: RS\nParent: Observation\nId: rs\n"
                               "* component ^slicing.discriminator[0].path = \"code\"\n* component ^slicing.rules = #open\n"
                               "* component contains Lab 0..*\n"
                               "* component[Lab] ^slicing.discriminator[0].path = \"code.coding\"\n"
                               "* component[Lab] ^slicing.rules = #open\n* component[Lab] contains Chem 0..1\n"))))
  (check-true (and (member "Observation.component:Lab/Chem" (ids rs)) #t))
  (check-equal? (fsh->fhir (fhir->fsh rs)) (list rs)))         ; round-trips

;; ---- snapshot generation (opt-in): differential merged onto the base's full snapshot ---------
(parameterize ([canonical-base "http://example.org"] [emit-snapshot? #t])
  (define sd (only (fsh->fhir "Profile: SnapPat\nParent: Patient\nId: snap-pat\n* name 1..*\n")))
  (define snap (hash-ref (hash-ref sd 'snapshot) 'element))
  (define paths (map (lambda (e) (hash-ref e 'path)) snap))
  ;; the base's inherited elements are present…
  (check-true (and (member "Patient.identifier" paths) #t))
  (check-true (and (member "Patient.gender" paths) #t))
  (check-true (> (length snap) 20))
  ;; …and the differential constraint is merged onto the base element
  (define name-el (findf (lambda (e) (equal? (hash-ref e 'path) "Patient.name")) snap))
  (check-equal? (hash-ref name-el 'min) 1))
