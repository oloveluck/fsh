#lang racket/base

;;; features-test.rkt — FSH features without a SUSHI golden pair (parameterized RuleSets, snapshot).

(require (only-in racket/list last) (only-in fhir/sd/sd-index sd-index-available?) rackunit
         racket/list
         (only-in fhir/r5/schemas)
         (only-in fsh/compile fsh->fhir canonical-base emit-snapshot?)
         (only-in fsh/decompile fhir->fsh))

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
  (check-equal? (hash-ref gender-el 'min) 1)
  (check-false (hash-has-key? gender-el 'max))   ; SUSHI writes what changes: Patient.gender is 0..1 already
  (check-equal? (hash-ref gender-el 'mustSupport) #t))

;; ---- Mapping: injects StructureDefinition.mapping + ElementDefinition.mapping into the Source ----
(parameterize ([canonical-base "http://example.org"])
  (define sds (fsh->fhir (string-append
                          "Profile: MyPat\nParent: Patient\nId: mypat\n* name 1..*\n\n"
                          "Mapping: M\nId: m\nSource: MyPat\nTarget: \"HL7 V2\"\nTitle: \"V2 Map\"\n"
                          "* identifier -> \"PID-3\"\n* name -> \"PID-5\" \"the name\"\n")))
  (define sd (only sds))                                ; the Mapping does not produce its own artifact
  (check-equal? (length sds) 1)
  ;; SUSHI: the parent's mapping declarations are inherited, the Mapping entity's is appended
  (check-equal? (last (hash-ref sd 'mapping)) (hasheq 'identity "m" 'name "V2 Map" 'uri "HL7 V2"))
  (when (sd-index-available?)
    (check-true (and (member "rim" (map (lambda (m) (hash-ref m 'identity)) (hash-ref sd 'mapping))) #t)))
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

;; ---- instances: SUSHI's slice positions, required values, code-typed primitives -----------------
(parameterize ([canonical-base "http://example.org"])
  (define out
    (fsh->fhir (string-append
                "Profile: Obs\nParent: Observation\nId: obs\n"
                "* code = http://loinc.org#1-1\n"                       ; Observation.code is 1..1 in the base
                "* component ^slicing.discriminator.type = #pattern\n"
                "* component ^slicing.discriminator.path = \"code\"\n"
                "* component ^slicing.rules = #open\n"
                "* component contains a 0..1 and b 0..1 and c 0..1\n"
                "* component[a].code = http://loinc.org#a\n"
                "* component[b].code = http://loinc.org#b\n"
                "* component[c].code = http://loinc.org#c\n\n"
                "Instance: o1\nInstanceOf: Obs\n"
                "* status = http://hl7.org/fhir/observation-status#final\n"  ; a code keeps only the code
                "* component[a].valueString = \"1\"\n"
                "* component[b].valueString = \"2\"\n"
                "* component[a].valueString = \"1\"\n"                   ; re-mentioning a…
                "* component[c].valueString = \"3\"\n")))                ; …does not let c take b's place
  (define o1 (findf (lambda (r) (equal? (hash-ref r 'resourceType) "Observation")) out))
  (check-equal? (hash-ref o1 'status) "final")
  (check-equal? (for/list ([c (in-list (hash-ref o1 'component))]) (hash-ref c 'valueString)) '("1" "2" "3"))
  ;; each named slice's entry gets the slice's required code; the base's required `code` its pattern
  (check-equal? (for/list ([c (in-list (hash-ref o1 'component))])
                  (hash-ref (car (hash-ref (hash-ref c 'code) 'coding)) 'code))
                '("a" "b" "c"))
  (check-equal? (hash-ref (car (hash-ref (hash-ref o1 'code) 'coding)) 'code) "1-1"))
