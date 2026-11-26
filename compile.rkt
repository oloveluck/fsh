#lang racket/base

;;; compile.rkt — FSH entity ASTs -> FHIR JSON (jsexpr).
;;;
;;; Adapted from the stroma FSH compiler to this repo's information model. Profiles/Extensions compile
;;; to a StructureDefinition DIFFERENTIAL (no snapshot — deferred); Instances to a plain resource
;;; (schema-driven array-vs-object nesting via resolve.rkt); ValueSet/CodeSystem to their compose /
;;; concept JSON; Invariant Expressions are checked with the FHIRPath parser. The target version is
;;; `fsh-fhir-version` (default "5.0.0") with element paths resolved against whichever tower the caller
;;; registered; canonical base default http://example.org, as in the SUSHI golden corpus.
;;;
;;; The jsexpr is the faithful output (compared against SUSHI); fsh/base.rkt additionally realises each
;;; as a typed node via from-json.
;;; The driver: documents (aliases, RuleSets, entities compiled with per-entity error isolation),
;;; entity dispatch, and the public API. The stages live beside it: context.rkt (parameters and
;;; helpers), sd.rkt (StructureDefinitions), instance.rkt (instances), caret-apply.rkt (carets),
;;; terminology.rkt (value sets and code systems).
(require racket/match
         racket/string
         racket/contract (only-in json jsexpr->string)
         (only-in racket/list append-map last filter-map remove-duplicates)
         "parser/ast.rkt"
         "parser/path.rkt"
         (only-in "parser/parse.rkt" parse-fsh-doc)
         (only-in fhir/diagnostic diagnostic diagnostic? diagnostic-message attached-loc attach-loc!)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/suggest did-you-mean)
         (only-in fhir/serialize/jsexpr-util drop-false)
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available? sd-context)
         (only-in fhir/sd/logical register-structure-definition!)
         "context.rkt"
         "instance.rkt"
         "sd.rkt"
         "terminology.rkt")
(provide (contract-out
          [canonical-base (parameter/c string?)]
          ;; a diagnostic of an instance without an element its InstanceOf requires, or with a value
          ;; it contradicts (`raco fsh check`, which validates the example and traces the same
          ;; problem to its rule, leaves it out)
          [required-element-diagnostic? (-> any/c boolean?)]
          [emit-snapshot? (parameter/c boolean?)]
          ;; the FHIR version stamped on generated StructureDefinitions/IGs ("5.0.0" by default; set
          ;; "4.0.1" — and register the R4 tower — to compile an R4 IG)
          [fsh-fhir-version (parameter/c string?)]
          [compile-entity (->* (struct?) (hash? hash? hash?) (or/c hash? #f))]
          [compile-doc    (-> (listof struct?) (listof hash?))]
          [compile-doc/errors (-> (listof struct?) (values (listof hash?) (listof (cons/c string? exn?))))]
          [entity-label (-> struct? string?)]   ; "profile: Name" — how compile-doc/errors names an entity
          ;; an instance's id: `Id:`, else an `* id = "…"` rule, else its name (the one definition
          ;; compile, check and pack share)
          [instance-id (-> fsh-instance? string?)]
          [compile-doc/diagnostics (-> (listof struct?) (values (listof hash?) (listof (cons/c string? exn?)) list?))]
          [fsh->fhir      (-> string? (listof hash?))]
          ;; the element path a profile rule constrains ("component[a].code"), or #f
          [rule-path-of   (-> any/c (or/c string? #f))]))
;; FSH source -> (listof FHIR jsexpr)
;; A program that has loaded no FHIR version gets the one `fsh-fhir-version` names (R5 unless set): the
;; compiler reads element types from the registered tower, and with none an Instance of a profile in
;; the same text was "InstanceOf not found". A tower already loaded is left as it is.
(define (fsh->fhir str)
  (unless (type-registered? 'Patient)
    (dynamic-require (if (regexp-match? #rx"^4[.]" (fsh-fhir-version)) 'fhir/r4/schemas 'fhir/r5/schemas) #f))
  (compile-doc (parse-fsh-doc str)))
;; compile a whole document: build alias/ruleset/profile tables, expand inserts, compile each entity
;; compile-doc : entities -> (listof jsexpr)   raises on the first entity that fails to compile
(define (compile-doc entities)
  (define-values (out errors) (compile-doc/errors entities))
  (when (pair? errors) (raise (cdr (car errors))))
  out)
;; compile-doc/diagnostics : entities -> (values resources errors diagnostics)   compile-doc/errors,
;; plus every rule-level diagnostic (fhir/diagnostic) — accumulated, compilation continues past them
(define (compile-doc/diagnostics entities)
  (define found (box '()))
  (define-values (out errors) (parameterize ([current-rule-diagnostics found]) (compile-doc/errors entities)))
  (values out errors (reverse (unbox found))))
;; current-invariant-names : (parameter (hash name -> any))   the Invariants of the document being compiled
(define current-invariant-names (make-parameter (hash)))
;; rule-diagnostic! : rule label message [#:severity] -> void   a problem at the rule, when collected
(define (rule-diagnostic! rule entity-label msg #:severity [severity 'error])
  (define b (current-rule-diagnostics))
  (when b (set-box! b (cons (diagnostic 'fsh 'compile severity (format "~a: ~a" entity-label msg) (attached-loc rule)) (unbox b)))))
;; check-element-paths! : entity -> void   a rule whose path starts at an element the base type does
;; not have (`* nosuchelement 1..1` on a Patient profile) is an error, positioned at the rule. Only
;; the FIRST path element is checked (deeper steps go through slices, extensions, choices and
;; datatypes this check does not model), and only when the base type's snapshot resolves — never a
;; guess (logical models, custom resources and unresolvable parents are skipped).
(define (check-element-paths! e)
  (when (current-rule-diagnostics)
    (define-values (base rules)
      (match e
        [(fsh-profile n m r) (values (instance-base (resolve-alias (or (assoc-ref m 'parent) "DomainResource") (current-aliases))
                                                    (current-profiles))
                                     r)]
        [(fsh-extension _ _ r) (values "Extension" r)]
        [(fsh-instance _ m r) (values (let ([of (assoc-ref m 'instanceOf)])
                                        (and of (instance-base (resolve-alias of (current-aliases)) (current-profiles))))
                                      r)]
        [_ (values #f '())]))
    ;; a binding to a value set nothing defines (not this project's, not its packages', not an alias,
    ;; not a url) is written as the bare name, which no validator resolves: say so at the rule
    (when (sd-index-available?)
      (for ([r (in-list rules)])
        (match r
          ;; (an error, as SUSHI's "Resolved value … is not a valid URI": a binding names a value
          ;; set by its canonical, and a bare name that resolves to nothing is none)
          [(rule-binding _ (? string? vs) _)
           (unless (regexp-match? #rx":" (valueset-url vs))
             (rule-diagnostic! r (entity-label e)
                               (format "value set `~a` not found: it is not a ValueSet of this project or its packages, an alias, or a url" vs)))]
          ;; a display in a code a PROFILE assigns is part of the pattern: every instance must then
          ;; carry exactly that display. Published profiles leave it out (88 of 92 pattern codings in
          ;; US Core, IPS and mCODE); written from habit (`= $loinc#82810-3 "Pregnancy status"`), it
          ;; over-constrains without a word
          [(rule-assign _ (fsh-code code (? string? display)) _)
           #:when (or (fsh-profile? e) (fsh-extension? e))
           (rule-diagnostic! r (entity-label e)
                             (format "the display \"~a\" becomes part of the pattern: every instance must carry exactly it. Write the code alone (`~a`) unless that is meant"
                                     display code)
                             #:severity 'warning)]
          ;; a type in an `only` rule that nothing defines: an error at the rule, with the nearest
          ;; name (it was written into the element as its type code)
          [(rule-only _ types)
           (for ([t (in-list types)] #:when (string? t))
             (define unknown (unknown-only-type t))
             (when unknown
               (rule-diagnostic! r (entity-label e)
                                 (format "type `~a` not found: it is not a FHIR type, nor a profile, extension or logical model of this project or its packages~a"
                                         unknown (did-you-mean unknown (parent-candidates))))))]
          [_ (void)])))
    (structure-rule-checks! e base rules)
    (when (or (fsh-profile? e) (fsh-extension? e)) (profile-path-checks! e base rules))
    (flag-rule-checks! e)
    (when (or (fsh-profile? e) (fsh-extension? e)) (extension-value-checks! e rules))
    (define names (and base (type-registered? (string->symbol base)) (base-element-names base)))
    (when names
      (for ([r (in-list rules)])
        (for ([p (in-list (cons (rule-path-of r) (match r [(rule-flag _ ps _) ps] [_ '()])))]
              #:when (and (string? p) (not (member p '("" "."))) (not (regexp-match? #rx"^[.^]" p))))
          (define first (let ([steps (with-handlers ([exn:fail? (lambda (_) '())]) (fsh-path-steps (parse-fsh-path p)))])
                          (and (pair? steps) (fsh-step-name (car steps)))))
          (when (and first (not (known-element? names first)))
            (rule-diagnostic! r (entity-label e) (format "`~a` is not an element of ~a~a" first base
                                                         (did-you-mean first (map (lambda (n) (regexp-replace #rx"\\[x\\]$" n "")) names))))))))))
;; profile-path-checks! : entity base-type rules -> void   what SUSHI cannot find at a profile's path
;; ("No element found at path …"):
;;   - an index or soft index (`item[0]`, `item[+]`): a profile states an element of every entry,
;;     not an entry — indexes belong to instances;
;;   - a type-specific choice name of a type the choice does not have (`valueDecimal` on an
;;     Observation, whose value[x] takes no decimal);
;;   - a slice nothing declares (`component[systolic].code` with no `contains systolic`), decided
;;     when the Parent is a base type (a Parent profile's slices are not read here); extension
;;     slices are named by their Extension and left alone.
(define (profile-path-checks! e base rules)
  (define label (entity-label e))
  (define parent-is-base?
    (or (fsh-extension? e)
        (and (fsh-profile? e)
             (let ([p (resolve-alias (or (assoc-ref (fsh-profile-metadata e) 'parent) "DomainResource") (current-aliases))])
               (and (not (hash-ref (current-profiles) p #f)) (equal? p base) (type-registered? (string->symbol p)))))))
  (define declared   ; path -> slice names this entity's `contains` rules declare there
    (for*/fold ([h (hash)]) ([r (in-list rules)] #:when (rule-contains? r) [it (in-list (rule-contains-items r))])
      (hash-update h (rule-contains-path r) (lambda (l) (cons (contains-item-name it) l)) '())))
  (define (bracket-text b)
    (match (fsh-bracket-kind b) ['index (format "[~a]" (fsh-bracket-value b))] ['soft+ "[+]"] ['soft= "[=]"]
      ['choice "[x]"] [_ (format "[~a]" (fsh-bracket-value b))]))
  (for ([r (in-list rules)])
    (define p (rule-path-of r))
    (define steps (and (string? p) (not (member p '("" "."))) (not (regexp-match? #rx"^[.^]" p))
                       (with-handlers ([exn:fail? (lambda (_) #f)]) (fsh-path-steps (parse-fsh-path p)))))
    (when steps
      (let loop ([steps steps] [before ""] [first? #t])
        (unless (null? steps)
          (define st (car steps))
          (define name (fsh-step-name st))
          (define here (if (string=? before "") name (string-append before "." name)))
          (define brackets (fsh-step-brackets st))
          (cond
            [(for/or ([b (in-list brackets)]) (and (memq (fsh-bracket-kind b) '(index soft+ soft=)) b))
             => (lambda (b)
                  (rule-diagnostic! r label
                                    (format "no element at `~a~a`: a profile constrains every entry of `~a`, not one of them (an index belongs to an instance)"
                                            here (bracket-text b) here)))]
            [(and first? (null? brackets) base (choice-type-name base name))
             => (lambda (bad)
                  (rule-diagnostic! r label
                                    (format "no element `~a`: ~a's `~a[x]` takes no ~a~a"
                                            name base (car bad) (cdr bad)
                                            (let ([ts (element-type-codes base (string-append (car bad) "[x]"))])
                                              (if (pair? ts) (format " (it takes ~a)" (string-join (filter string? ts) ", ")) ""))))
                  (loop (cdr steps) (string-append before (if (string=? before "") "" ".") name) #f))]
            [(and parent-is-base? (not (member name '("extension" "modifierExtension")))
                  (for/or ([b (in-list brackets)])
                    (and (eq? (fsh-bracket-kind b) 'slice) (string? (fsh-bracket-value b))
                         (not (regexp-match? #rx"/" (fsh-bracket-value b)))
                         (not (member (fsh-bracket-value b) (hash-ref declared here '())))
                         (fsh-bracket-value b))))
             => (lambda (slice)
                  (rule-diagnostic! r label
                                    (format "no slice `~a` of `~a`: declare it first, `* ~a contains ~a 0..1`" slice here here slice)))]
            [else (loop (cdr steps) (string-append here (apply string-append (map bracket-text brackets))) #f)]))))))
;; choice-type-name : base-type name -> (cons stem type) | #f   a type-specific choice name
;; (`valueDecimal`) whose type the base's choice element (`value[x]`) does not take; #f when it does,
;; when the name is no such name, or when the base's types cannot be read
(define (choice-type-name base name)
  (define m (regexp-match #px"^([a-z][A-Za-z0-9]*?)([A-Z][A-Za-z0-9]*)$" name))
  (and m
       (let* ([names (or (base-element-names base) '())])
         (and (not (member name names))
              (for/or ([split (in-range 1 (string-length name))])
                (define stem (substring name 0 split))
                (define type (substring name split))
                (and (char-upper-case? (string-ref type 0))
                     (member (string-append stem "[x]") names)
                     (let ([ts (element-type-codes base (string-append stem "[x]"))])
                       (and (list? ts) (pair? ts)
                            (not (for/or ([t (in-list ts)])
                                   (and (string? t) (string-ci=? t type))))
                            (cons stem type)))))))))
;; element-type-codes : base-type path -> (listof string) | #f   the type codes of the base's element
(define (element-type-codes base path)
  (define el (base-element-of base path))
  (and (hash? el) (for/list ([t (in-list (hash-ref el 'type '()))]) (hash-ref t 'code #f))))
;; extension-value-checks! : entity rules -> void   an extension has a value or extensions, not
;; both (FHIR's ext-1; SUSHI: "Extension on X cannot have both a value and sub-extensions"). SUSHI's
;; walk, rule by rule in order: at each step of a rule's path that is on an extension (the first
;; step of an Extension's own rule, or a step after an `extension…` step), a rule on `extension…`
;; there marks `value[x]` as to be left out, and one on `value…` marks `extension` the same way; a
;; rule that meets the other's mark is the contradiction, unless it is a cardinality of max 0 (which
;; only clears the mark). A step not on an extension ends the walk of that rule.
(define (extension-value-checks! e rules)
  (define label (entity-label e))
  (for/fold ([marks (hash)]) ([r (in-list rules)])
    (define p (rule-target-path r))
    (define max0? (match r [(rule-card _ (? string? c) _) (regexp-match? #rx"[.][.]0$" c)] [_ #f]))
    (define steps (if (and (string? p) (not (member p '("" ".")))) (path-steps p) '()))
    (let loop ([i 0] [before '()] [marks marks])
      (cond
        [(>= i (length steps)) marks]
        [else
         (define step (list-ref steps i))
         (define on-extension? (or (and (fsh-extension? e) (= i 0))
                                   (and (pair? before) (string-prefix? (car before) "extension"))))
         (define base (apply string-append (map (lambda (s) (string-append s ".")) (reverse before))))
         (define (meet own other)
           ;; own: the mark this rule's kind meets; other: the mark it sets
           (define key (string-append base own))
           (cond [max0? (if (hash-ref marks key #f) (hash-set marks key #f) marks)]
                 [(hash-ref marks key #f)
                  (rule-diagnostic! r label "an extension has a value or sub-extensions, not both: this rule gives it the one, an earlier rule the other (constrain the one not used to 0..0)")
                  (hash-set marks key #f)]
                 [else (hash-set marks (string-append base other) #t)]))
         (cond
           [(not on-extension?) marks]
           [(string-prefix? step "extension") (loop (add1 i) (cons step before) (meet "extension" "value[x]"))]
           [(string-prefix? step "value") (loop (add1 i) (cons step before) (meet "value[x]" "extension"))]
           [else (loop (add1 i) (cons step before) marks)])])))
  (void))
;; path-steps : path -> (listof string)   a rule path's steps, split at dots outside brackets
;; (`extension[http://x.org/y].value[x]` is two)
(define (path-steps p)
  (let loop ([cs (string->list p)] [depth 0] [cur '()] [acc '()])
    (cond [(null? cs) (reverse (cons (list->string (reverse cur)) acc))]
          [(and (char=? (car cs) #\.) (zero? depth)) (loop (cdr cs) depth '() (cons (list->string (reverse cur)) acc))]
          [else (loop (cdr cs) (case (car cs) [(#\[) (add1 depth)] [(#\]) (max 0 (sub1 depth))] [else depth])
                      (cons (car cs) cur) acc)])))
;; flag-rule-checks! : entity -> void   what SUSHI's exporter refuses of flags and added elements:
;;   - two standards statuses on one element (`TU N`: "Cannot apply multiple standards status");
;;   - MustSupport in a logical model or a custom resource (a specialization: "The MustSupport
;;     flag is not permitted … allowed only in profiles and extensions");
;;   - an added element of several types whose name does not end in `[x]` (a choice element),
;;     unless every type is a Reference, a Canonical or a CodeableReference (several targets).
(define (flag-rule-checks! e)
  (define label (entity-label e))
  (define specialization? (or (fsh-logical? e) (fsh-resource? e)))
  (for ([r (in-list (or (entity-rules e) '()))])
    (define flags (match r
                    [(rule-flag _ _ fl) fl] [(rule-card _ _ fl) fl]
                    [(rule-add-element _ _ fl _ _ _) fl] [(rule-add-cr-element _ _ fl _ _ _) fl]
                    [(rule-contains _ items) (append-map contains-item-flags items)]
                    [_ '()]))
    (when (> (length (filter (lambda (f) (memq f '(TU N D))) (remove-duplicates flags))) 1)
      (rule-diagnostic! r label "an element has one standards status: give one of TU, N and D, not several"))
    (when (and specialization? (memq 'MS flags))
      (rule-diagnostic! r label
                        (format "MustSupport (MS) is not permitted in ~a: it says what a system using a profile must support, so it belongs in profiles and extensions"
                                (if (fsh-logical? e) "a logical model" "a custom resource"))))
    (match r
      [(rule-add-element (? string? p) _ _ (? list? types) _ _)
       #:when (and (> (length types) 1) (not (regexp-match? #rx"\\[x\\]$" p))
                   (not (for/or ([kind (in-list '("Reference(" "Canonical(" "CodeableReference("))])
                          (andmap (lambda (t) (and (string? t) (string-prefix? t kind))) types))))
       (rule-diagnostic! r label
                         (format "`~a` has several types, so it is a choice element: name it `~a[x]`" p p))]
      [_ (void)])))
;; structure-rule-checks! : entity base-type rules -> void   what SUSHI refuses in a profile's rules
;; and this compiler wrote without a word (found by running both over SUSHI's own test inputs,
;; fhir-examples' value experiment H part 3):
;;   - an invariant an `obeys` names that no Invariant of the project defines;
;;   - an extension slice with no Extension behind it (`* extension contains foo 0..1`) outside an
;;     Extension: a profile names the Extension, `contains <Extension> named <slice>`;
;;   - a cardinality that does not fit within the base type's (`0..*` on an element that is 0..1,
;;     a minimum below the base's);
;;   - `contains` on an element nothing says how to slice.
;; The last two are decided only for a profile whose Parent is a base type and a plain path (a
;; Parent profile may have narrowed or sliced the element: not modelled here, so nothing is said).
(define (structure-rule-checks! e base rules)
  (define label (entity-label e))
  (define profile? (fsh-profile? e))
  (define parent-is-base?
    (and profile?
         (let ([p (resolve-alias (or (assoc-ref (fsh-profile-metadata e) 'parent) "DomainResource") (current-aliases))])
           (and (not (hash-ref (current-profiles) p #f)) (equal? p base) (type-registered? (string->symbol p))))))
  (define (extension-rule-path? p) (and (string? p) (regexp-match? #px"(^|[.])(extension|modifierExtension)$" p)))
  (define sliced-here   ; the paths this entity states a slicing for
    (for/list ([r (in-list rules)] #:when (and (rule-caret? r) (string? (rule-caret-caret-path r))
                                               (regexp-match? #rx"^\\^?slicing([.]|$)" (rule-caret-caret-path r))))
      (rule-caret-path r)))
  (when (or profile? (fsh-extension? e))
    (for ([r (in-list rules)])
      (match r
        [(rule-obeys _ invs)
         (for ([i (in-list invs)] #:when (and (string? i) (not (hash-ref (current-invariant-names) i #f))))
           (rule-diagnostic! r label (format "invariant `~a` not found: no Invariant of that name is in the project~a"
                                             i (did-you-mean i (hash-keys (current-invariant-names))))))]
        [(rule-contains (? extension-rule-path? p) items)
         #:when profile?
         (for ([it (in-list items)] #:unless (contains-item-named-as it))
           (rule-diagnostic! r label
                             (format "`~a` names no Extension: in a profile an extension slice is `~a contains <Extension> named ~a`; an extension with no definition of its own can only be declared inside an Extension"
                                     (contains-item-name it) p (contains-item-name it))))]
        [(rule-contains (? string? p) items)
         #:when (and parent-is-base? (not (extension-rule-path? p)) (not (member p sliced-here)))
         (define el (base-element-of base p))
         (when (and el (not (hash-ref el 'slicing #f)))
           (rule-diagnostic! r label
                             (format "cannot create the slice `~a`: `~a` is not sliced. State how its entries are told apart first: `* ~a ^slicing.discriminator.type = #pattern`, `* ~a ^slicing.discriminator.path = \"<element>\"`, `* ~a ^slicing.rules = #open`"
                                     (contains-item-name (car items)) p p p p)))]
        [(rule-card (? string? p) (? string? card) _)
         #:when parent-is-base?
         (define el (base-element-of base p))
         (define m (regexp-match #px"^([0-9]*)[.][.]([0-9]*|[*])$" card))
         (when (and el m (exact-nonnegative-integer? (hash-ref el 'min #f)) (string? (hash-ref el 'max #f)))
           (define bmin (hash-ref el 'min))
           (define bmax (hash-ref el 'max))
           (define lo (and (positive? (string-length (cadr m))) (string->number (cadr m))))
           (define hi (and (positive? (string-length (caddr m))) (caddr m)))
           (define (wider? hi) (and (not (equal? bmax "*")) (or (equal? hi "*") (> (string->number hi) (string->number bmax)))))
           (when (or (and lo (< lo bmin)) (and hi (wider? hi)))
             (rule-diagnostic! r label
                               (format "cannot constrain `~a` to ~a: it does not fit within ~a's ~a..~a (a profile narrows a cardinality, it cannot widen it)"
                                       p card base bmin bmax))))]
        [_ (void)]))))

;; base-element-names : type -> (listof string)|#f   its top-level element names ("value[x]", "name"),
;; per package context (every entity of a type asks; the snapshot scan was repeated for each)
(define base-element-name-cache (make-hash))
(define (base-element-names type)
  (hash-ref! base-element-name-cache (cons (and (sd-index-available?) (sd-context)) type)
             (lambda () (scan-base-element-names type))))
(define (scan-base-element-names type)
  (define sd (and (sd-index-available?) (resolve-sd-raw type)))
  (define els (and (hash? sd) (hash-ref (hash-ref sd 'snapshot (hash)) 'element #f)))
  (define rx (pregexp (string-append "^" (regexp-quote type) "[.]([^.:]+)$")))
  (and (list? els)
       (for*/list ([el (in-list els)]
                   [m (in-value (regexp-match rx (hash-ref el 'path "")))]
                   #:when m)
         (cadr m))))
;; known-element? : names name -> boolean   directly, as a choice ("value[x]"), or a choice spelled
;; with its type ("valueQuantity")
(define (known-element? names n)
  (or (member n names)
      (member (string-append n "[x]") names)
      (for/or ([c (in-list names)] #:when (regexp-match? #rx"\\[x\\]$" c))
        (define stem (substring c 0 (- (string-length c) 3)))
        (and (> (string-length n) (string-length stem)) (string-prefix? n stem)
             (char-upper-case? (string-ref n (string-length stem)))))
      #f))
;; compile-doc/errors : entities -> (values (listof jsexpr) (listof (cons entity-label exn)))
;; Like SUSHI, a failing entity is reported and the rest of the document still compiles.
(define (compile-doc/errors entities0)
  (define entities (map sanitized-id entities0))
  (define aliases (alias-table entities))
  (define rulesets (ruleset-table entities))
  (define param-rulesets (param-ruleset-table entities))
  (define profiles (profile-table entities))
  (define invariants (invariant-table entities rulesets param-rulesets))
  (define instances (instance-table entities))
  (define logicals (logical-table entities))
  (define local-canonicals (local-canonical-table entities))
  ;; compile-one : entity -> jsexpr   an entity with its inserts expanded, compiled (raises)
  (define (compile-one e)
    (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants))
  ;; Instances by name and id, compiled on demand so one instance can be embedded in another;
  ;; `Usage: #inline` instances are ONLY embedded, never emitted on their own.
  (define resolve-instance (compiled-on-demand instances compile-one))
  ;; this document's logical models and custom resources are registered as types (fhir/sd/logical,
  ;; the same registry the FHIR towers fill), so an Instance of one builds like any resource
  (for ([e (in-list entities)] #:when (or (fsh-logical? e) (fsh-resource? e)))
    (define sd (with-handlers ([exn:fail? (lambda (_) #f)])
                 (parameterize ([current-aliases aliases] [current-logicals logicals])
                   (compile-one e))))
    (when (hash? sd) (register-structure-definition! sd)))
  ;; this document's Profiles (and logical models) compiled on demand (name/id/url ->
  ;; StructureDefinition jsexpr), for the required values an Instance inherits from its InstanceOf
  ;; profile and the characteristics a logical model inherits from its Parent
  (define resolve-profile-sd
    (compiled-on-demand (definition-table entities)
                        (lambda (e) (parameterize ([emit-snapshot? #f]) (compile-one e)))))
  (define slice-orders-of (slice-orders (own-slice-orders entities) profiles))
  ;; instance name/id -> "Type/id", from metadata alone (no compilation, so references never cycle)
  (define (instance-ref name)
    (define e (hash-ref instances name #f))
    (and e (let* ([of (let ([o (assoc-ref (fsh-instance-metadata e) 'instanceOf)]) (and o (resolve-alias o aliases)))]
                  [base (and of (instance-base of profiles))])
             (and base (string-append base "/" (instance-id e))))))
  (define (inline? e) (and (fsh-instance? e) (equal? (assoc-ref (fsh-instance-metadata e) 'usage) "#inline")))
  ;; Invariants are consumed only as SD constraint sources (like SUSHI) — not emitted standalone.
  ;; Each entity is (cons jsexpr #f) or (cons #f (cons label exn)): a RuleSet that fails to expand
  ;; (its instantiated body does not parse) fails that entity alone, like any other compile error.
  (define (attempt e thunk)
    (with-handlers ([exn:fail? (lambda (x) (cons #f (cons (entity-label e) x)))]) (cons (thunk) #f)))
  (define outcomes
    (parameterize ([current-instance-resolver resolve-instance] [current-instance-ref instance-ref]
                   [current-profile-sd resolve-profile-sd]
                   [current-aliases aliases] [current-profiles profiles] [current-logicals logicals]
                   [current-invariant-names invariants]
                   [current-slice-orders slice-orders-of]
                   [current-local-canonical (lambda (n) (hash-ref local-canonicals n #f))])
      (for/list ([e (in-list entities)]
                 #:unless (or (fsh-alias? e) (fsh-ruleset? e) (fsh-param-ruleset? e)
                              (fsh-invariant? e) (fsh-mapping? e)))
        (match (attempt e (lambda () (expand-entity-inserts e rulesets param-rulesets)))
          [(cons #f failure) (cons #f failure)]
          [(cons x _)
           (check-element-paths! x)                               ; rule-level diagnostics (accumulated)
           (if (inline? e)
               (cons #f #f)
               (attempt e (lambda () (compile-entity x aliases profiles invariants))))]))))
  ;; Mapping entities inject into their Source StructureDefinition (SUSHI-style) — a post-pass,
  ;; their own `insert`s expanded first (a Mapping's rules may come from a RuleSet)
  ;; a Mapping's Source is a definition of the document (SUSHI: "Unable to find source"); one with
  ;; no Source, or one nothing defines, was dropped without a word
  (define structure-names
    (for*/list ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e))
                [k (in-list (list (entity-name e) (assoc-ref (entity-metadata e) 'id)))] #:when k)
      k))
  (define mappings (for/list ([m (in-list entities)] #:when (fsh-mapping? m))
                     (attempt m (lambda ()
                                  (define src (assoc-ref (fsh-mapping-metadata m) 'source))
                                  (unless (and src (member (resolve-alias src aliases) structure-names))
                                    (raise (exn:fail (if src
                                                         (format "mapping ~a: its Source `~a` is no profile, extension or logical model of this project" (entity-name m) src)
                                                         (format "mapping ~a has no Source: say which definition it maps with `Source:`" (entity-name m)))
                                                     (current-continuation-marks))))
                                  (expand-entity-inserts m rulesets param-rulesets)))))
  (values (for/fold ([sds (filter values (map car outcomes))]) ([m (in-list (filter values (map car mappings)))])
            (apply-mapping sds m))
          (filter values (map cdr (append outcomes mappings)))))

;; sanitized-id : entity -> entity   SUSHI's `validateId` for a definition (not an instance): an id
;; (its `Id:`, else its name) that is no FHIR id but a valid FHIR name is written with `-` for each
;; `_`, with a warning — `ValueSet: SimpleVS` with `Id: SimpleVS_ID` is ValueSet/SimpleVS-ID. A
;; `^id` caret rule leaves the id as it states (SUSHI: findAssignmentByPath 'id').
(define (sanitized-id e)
  (define m (entity-metadata e))
  (define id (and (or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e)
                      (fsh-valueset? e) (fsh-codesystem? e))
                  (let ([v (or (assoc-ref m 'id) (entity-name e))]) (and (string? v) v))))
  (define sanitized (and id (regexp-replace* #rx"_" id "-")))
  (cond
    [(and id (not (fhir-id? id)) (regexp-match? #px"^[A-Z][A-Za-z0-9_]{0,254}$" id)
          (> (string-length sanitized) 0) (fhir-id? (substring sanitized 0 (min 64 (string-length sanitized))))
          (not (for/or ([r (in-list (or (entity-rules e) '()))])
                 (match r [(rule-caret (or #f "" ".") "id" _) #t] [_ #f]))))
     (define new-id (substring sanitized 0 (min 64 (string-length sanitized))))
     (rule-diagnostic! e (entity-label e)
                       (format "`~a` is a FHIR name but not a FHIR id (an id has no `_` and at most 64 characters): it is written as `~a`. Give the id with `Id:` to say it yourself"
                               id new-id)
                       #:severity 'warning)
     (with-metadata e (cons (cons 'id new-id) (filter (lambda (kv) (not (eq? (car kv) 'id))) m)))]
    [else e]))
(define (fhir-id? s) (regexp-match? #px"^[A-Za-z0-9.-]{1,64}$" s))
;; with-metadata : entity metadata -> entity   the entity with its keyword lines replaced
(define (with-metadata e m)
  (match e
    [(fsh-profile n _ r) (fsh-profile n m r)] [(fsh-extension n _ r) (fsh-extension n m r)]
    [(fsh-logical n _ r) (fsh-logical n m r)] [(fsh-resource n _ r) (fsh-resource n m r)]
    [(fsh-valueset n _ r) (fsh-valueset n m r)] [(fsh-codesystem n _ r) (fsh-codesystem n m r)]
    [_ e]))

;; ---- a document's tables: what its entities declare, by the names rules use for them --------------
;; alias-table : entities -> (hash name -> value)
(define (alias-table entities)
  (for/hash ([e (in-list entities)] #:when (fsh-alias? e)) (values (fsh-alias-name e) (fsh-alias-value e))))
;; ruleset-table : entities -> (hash name -> rules)
(define (ruleset-table entities)
  (for/hash ([e (in-list entities)] #:when (fsh-ruleset? e)) (values (fsh-ruleset-name e) (fsh-ruleset-rules e))))
;; param-ruleset-table : entities -> (hash name -> (cons params body-tokens))
(define (param-ruleset-table entities)
  (for/hash ([e (in-list entities)] #:when (fsh-param-ruleset? e))
    (values (fsh-param-ruleset-name e) (cons (fsh-param-ruleset-params e) (fsh-param-ruleset-body e)))))
;; structure-keys : entity -> (listof string)   what names a StructureDefinition entity: its name,
;; its id and its canonical url — SUSHI resolves InstanceOf and Parent by any of the three
(define (structure-keys e)
  (define id (entity-id e))
  (remove-duplicates (list (entity-name e) id (string-append (canonical-base) "/StructureDefinition/" id))))
;; profile-table : entities -> (hash name|id|url -> (cons parent id))   the Profiles and Extensions,
;; for an Instance's base type and meta.profile
(define (profile-table entities)
  (for*/hash ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e))
              [key (in-list (structure-keys e))])
    (values key (cons (or (assoc-ref (entity-metadata e) 'parent) (if (fsh-extension? e) "Extension" "DomainResource"))
                      (entity-id e)))))
;; definition-table : entities -> (hash name|id|url -> entity)   every StructureDefinition entity
(define (definition-table entities)
  (for*/hash ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e))
              [k (in-list (structure-keys e))])
    (values k e)))
;; logical-table : entities -> (hash name|id|url -> canonical url)   logical models and custom resources
(define (logical-table entities)
  (for*/hash ([e (in-list entities)] #:when (or (fsh-logical? e) (fsh-resource? e))
              [key (in-list (structure-keys e))])
    (values key (string-append (canonical-base) "/StructureDefinition/" (entity-id e)))))
;; local-canonical-table : entities -> (hash name|id -> canonical url)   this document's ValueSets
;; and CodeSystems
(define (local-canonical-table entities)
  (for*/hash ([e (in-list entities)] #:when (or (fsh-valueset? e) (fsh-codesystem? e))
              [k (in-list (remove-duplicates (list (entity-name e) (entity-id e))))])
    (values k (string-append (canonical-base) "/" (if (fsh-valueset? e) "ValueSet" "CodeSystem") "/" (entity-id e)))))
;; invariant-table : entities rulesets param-rulesets -> (hash name -> constraint source)
;; an Invariant's fields come from its keywords (`Severity:`) OR from assignment rules
;; (`* severity = #error`, `* expression = "…"`) — both forms are FSH; its rules may come from a RuleSet
(define (invariant-table entities rulesets param-rulesets)
  (for/hash ([e (in-list entities)] #:when (fsh-invariant? e))
    (define m (fsh-invariant-metadata e))
    (define rules (expand-rules (fsh-invariant-rules e) rulesets param-rulesets))
    (define (rule-val k)
      (for/or ([r (in-list rules)])
        (match r [(rule-assign (== k) v _) (match v [(struct fsh-code (c _)) c] [_ v])] [_ #f])))
    (define (field k [rk (symbol->string k)]) (or (assoc-ref m k) (rule-val rk)))
    (values (fsh-invariant-name e)
            (drop-false
             (hasheq 'severity (strip# (or (field 'severity) "#error"))
                     'human (or (field 'description) (rule-val "human"))
                     'expression (field 'expression)
                     'xpath (field 'xpath))))))
;; instance-table : entities -> (hash name|id -> instance)   names take precedence over ids (an id
;; may be shared by instances of different types); of two instances with one id the first wins
(define (instance-table entities)
  (define instances (filter fsh-instance? entities))
  (define by-id (for/fold ([h (hash)]) ([e (in-list instances)])
                  (if (hash-has-key? h (instance-id e)) h (hash-set h (instance-id e) e))))
  (for/fold ([h by-id]) ([e (in-list instances)]) (hash-set h (fsh-instance-name e) e)))
;; compiled-on-demand : (hash key -> entity) (entity -> jsexpr) -> (key -> jsexpr | #f)   the entity
;; of that key compiled, once (memoised by entity); #f for no such entity, for one that fails to
;; compile, and for one being compiled right now (a cycle stays a name)
(define (compiled-on-demand table compile-one)
  (define memo (make-hasheq))
  (lambda (key)
    (define e (hash-ref table key #f))
    (cond [(not e) #f]
          [(hash-has-key? memo e) (hash-ref memo e)]
          [else (hash-set! memo e #f)
                (define js (with-handlers ([exn:fail? (lambda (_) #f)]) (compile-one e)))
                (hash-set! memo e js)
                js])))
;; own-slice-orders : entities -> (hash profile name|id -> (hash element-path -> REQUIRED slice
;; names in declaration order))   from each profile's own `contains` rules
(define (own-slice-orders entities)
  (for*/hash ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e))
              [k (in-list (remove-duplicates (list (entity-name e) (entity-id e))))])
    (values k (for/fold ([h (hash)]) ([r (in-list (entity-rules e))] #:when (rule-contains? r))
                (hash-update h (rule-contains-path r)
                             (lambda (l) (append l (for/list ([it (in-list (rule-contains-items r))]
                                                              #:when (let ([c (contains-item-card it)]) (and (string? c) (>= (card-lo c) 1))))
                                                     (or (contains-item-named-as it) (contains-item-name it)))))
                             '())))))
;; slice-orders : own-slice-orders profiles -> (profile -> (hash element-path -> slice names))
;; a profile's required slices, its Parents' first. SUSHI creates a profile's required slices before
;; applying an instance's rules (in profile order); every other slice takes its place in the order
;; the instance first mentions it.
(define ((slice-orders own profiles) name [fuel 16])
  (define parent (let ([p (hash-ref profiles name #f)]) (and p (car p))))
  (define inherited (if (and parent (> fuel 0) (hash-ref own parent #f)) ((slice-orders own profiles) parent (sub1 fuel)) (hash)))
  (for/fold ([h inherited]) ([(k v) (in-hash (hash-ref own name (hash)))]) (hash-update h k (lambda (l) (append l v)) '())))
;; entity-label : entity -> string   "Profile: X" etc., for error reports
(define (entity-label e)
  (define v (struct->vector e))
  (format "~a: ~a" (regexp-replace #rx"^struct:fsh-" (symbol->string (vector-ref v 0)) "")
          (if (> (vector-length v) 1) (vector-ref v 1) "?")))
;; entity-id : entity -> string   `Id:`, else the name (a definition's id; an instance's is instance-id)
(define (entity-id e) (or (assoc-ref (entity-metadata e) 'id) (entity-name e)))
;; instance-id : instance -> string   `Id:`, else an `* id = "…"` rule, else its name
(define (instance-id e)
  (or (assoc-ref (fsh-instance-metadata e) 'id)
      (for/or ([r (in-list (fsh-instance-rules e))])
        (match r [(rule-assign "id" (? string? v) _) v] [_ #f]))
      (fsh-instance-name e)))
;; ---- insert / RuleSet expansion (plain + parameterized) ------------------------------------
(define (expand-rules rules rulesets param-rulesets)
  (append-map
   (lambda (r0)
     ;; inside a RuleSet the grammar reads `* insert X` as a code-insert with no codes: the same rule
     (define r (match r0 [(rule-code-insert '() ref) (rule-insert #f ref)] [_ r0]))
     ;; an insert under a concept (`* #cookie insert MyRuleSet`) names a RuleSet like any insert
     (match r
       [(rule-code-insert (? pair?) ref)
        (define name (if (pair? ref) (car ref) ref))
        (when (and (string? name) (not (hash-ref rulesets name #f)) (not (hash-ref param-rulesets name #f)))
          (unknown-ruleset! r name))]
       [_ (void)])
     (cond
       ;; plain `insert RuleSet` — ref is the ruleset name string
       [(and (rule-insert? r) (string? (rule-insert-ref r)))
        (define rs (hash-ref rulesets (rule-insert-ref r) #f))
        (unless (or rs (hash-ref param-rulesets (rule-insert-ref r) #f)) (unknown-ruleset! r (rule-insert-ref r)))
        (if rs (under (rule-insert-path r) (expand-rules rs rulesets param-rulesets)) (list r))]
       ;; parameterized `insert RuleSet(args)` — ref is (name param-token …)
       [(and (rule-insert? r) (pair? (rule-insert-ref r)))
        (define ref (rule-insert-ref r))
        (define pr (hash-ref param-rulesets (car ref) #f))
        (cond
          [(not pr) (unknown-ruleset! r (car ref)) (list r)]
          [else
           (define args (filter-map param-arg (cdr ref)))
           (under (rule-insert-path r)
                  (expand-rules (instantiate-ruleset (car pr) (cdr pr) args #:at (attached-loc r)) rulesets param-rulesets))])]
       [else (list r)]))
   rules))
;; under : path|#f rules -> rules   `* sectionProblems insert M` (or an insert indented under a rule)
;; applies the RuleSet's rules under that path: a rule's path is joined to it, a path-less one (an
;; SD-level caret) becomes a rule on the element itself
(define (under prefix rules)
  (if (and (string? prefix) (not (member prefix '("" "."))))
      (map (lambda (r) (prefix-rule prefix r)) rules)
      rules))
;; prefix-rule : path rule -> rule   the rule under the path, still known to be written where it was
(define (prefix-rule prefix r)
  (define r* (map-rule-path (lambda (p) (if (or (not p) (equal? p "") (equal? p ".")) prefix (string-append prefix "." p))) r))
  (define l (attached-loc r))
  (when (and l (not (eq? r* r))) (attach-loc! r* l))
  r*)
;; a (parameter|lastParameter "v,") token -> its cleaned argument value
(define (param-arg x)
  (match x
    [(list (or 'parameter 'lastParameter) v)
     ;; the lexeme ends with its one terminator (`,` or `)`); an escaped `\)` before it stays
     (define body (regexp-replace #px"\\s*[),]$" (string-trim v) ""))
     (match (regexp-match #px"^\\[\\[(.*)\\]\\]$" body)
       [(list _ inner) inner]                                 ; [[…]] is verbatim
       [#f (unescape-param (string-trim body))])]
    [_ #f]))
;; unescape-param : string -> string   a `)` or `,` inside an argument is written `\)` / `\,` (and a
;; backslash `\\`): the backslash goes, the character stays
(define (unescape-param s) (regexp-replace* #px"\\\\([),\\\\])" s "\\1"))
;; instantiate-ruleset : params body-tokens args [#:at loc] -> rules
;; a parameterized RuleSet instantiated: {param}->arg substituted in the raw body, the text read as
;; rules again (lexed; the parse itself is a template the parser remembers by token shape, so the
;; thousands of inserts of a guide — mCODE: 866 — cost a lexing each).
;; Each rule is located at the INSERT (#:at): its own position is in a text nobody can open, and a
;; problem in it (`* insert Req(nmae)`: no element `nmae`) is fixed where the argument is written.
(define (instantiate-ruleset params body args #:at [at #f])
  (define subst (substitution params args))
  ;; a STAR token carries its own leading "\n* " (starts a rule line); other tokens get a space.
  ;; padding a STAR would let the WS lexer eat the newline the STAR rule needs.
  (define text (apply string-append "RuleSet: __expand"
                      (for/list ([t (in-list body)])
                        (define raw (let ([x (cdr t)]) (if (code-tok? x) (code-tok-text x) x)))
                        (define v (requote (car t) (substitute raw subst)))
                        (if (eq? (car t) 'STAR) v (string-append " " v)))))
  (define rules (match (parse-fsh-doc text) [(cons (? fsh-ruleset? rs) _) (fsh-ruleset-rules rs)] [_ '()]))
  (when at (for ([r (in-list rules)]) (attach-loc! r at)))
  rules)
;; requote : token-type string -> string   a body token's value back as source text: the lexer
;; strips the quotes of strings and units (`"Not used in this profile"` re-lexed bare would be words)
(define (requote type v)
  (case type
    [(STRING) (string-append "\"" (regexp-replace* #rx"[\\\"]" v "\\\\&") "\"")]
    [(MULTILINE_STRING) (string-append "\"\"\"" v "\"\"\"")]
    [(UNIT) (string-append "'" v "'")]
    ;; a nested parameterized insert: the reference was cleaned of its `(`, which opens the arguments
    [(PARAM_RULESET_REFERENCE) (string-append v "(")]
    ;; a STAR's lexeme can carry the comment lines before it (`// note\n* `): keep only its own line
    [(STAR) (string-append "\n" (last (string-split (string-append v " ") "\n")))]
    [else v]))
;; A Substitution is (listof (cons regexp arg)): each `{param}`'s pattern with its argument, in
;; parameter order. Built once per instantiation (the patterns were compiled again for every token).
;; substitution : params args -> Substitution
(define (substitution params args)
  (for/list ([p (in-list params)] [a (in-list args)])
    (cons (regexp (regexp-quote (string-append "{" p "}"))) a)))
;; substitute : string Substitution -> string   each `{param}` becomes its argument, in order,
;; verbatim (a procedure, so a `\` or `&` in the argument is not read as a replacement pattern)
(define (substitute s subst)
  (for/fold ([s s]) ([kv (in-list subst)])
    (if (regexp-match? (car kv) s) (regexp-replace* (car kv) s (lambda (_) (cdr kv))) s)))
;; unknown-ruleset! : rule name -> void   an insert of a RuleSet the project does not define is an
;; error at the rule (SUSHI: "Unable to find definition for RuleSet"); the rule is left as it is
(define current-insert-label (make-parameter #f))
(define (unknown-ruleset! r name)
  (when (current-insert-label)
    (rule-diagnostic! r (current-insert-label) (format "RuleSet `~a` not found: no RuleSet of that name is in the project" name))))
;; expand-entity-inserts : entity rulesets param-rulesets -> entity   a definition or instance with
;; every `insert` replaced by the RuleSet's rules (an Invariant's and a RuleSet's own rules are
;; expanded where they are used)
(define (expand-entity-inserts e rulesets param-rulesets)
  (if (or (fsh-invariant? e) (fsh-ruleset? e))
      e
      (with-entity-rules e (lambda (rs) (parameterize ([current-insert-label (entity-label e)])
                                          (expand-rules rs rulesets param-rulesets))))))
;; ---- entity dispatch -----------------------------------------------------------------------
;; check-id! : entity -> void   the id an entity's resource gets — its `Id:`, an instance's `* id =`,
;; else its name — is a FHIR id: letters, digits, `-` and `.`, at most 64 characters. Otherwise an
;; error at the entity (SUSHI: "does not represent a valid FHIR id"): no server stores such a
;; resource. An underscore is left to pass: SUSHI writes `-` for it, with a warning.
(define (check-id! e)
  (define id
    (cond [(fsh-instance? e) (instance-id e)]
          [(or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e) (fsh-valueset? e) (fsh-codesystem? e))
           (or (assoc-ref (entity-metadata e) 'id) (entity-name e))]
          [else #f]))
  (define text (cond [(string? id) id] [(number? id) (number->string id)] [else #f]))
  (when (and text (not (regexp-match? #px"^[A-Za-z0-9.-]{1,64}$" (regexp-replace* #rx"_" text "-"))))
    (raise (exn:fail (format "`~a` is not a FHIR id: an id is letters, digits, `-` and `.`, at most 64 characters. Give the ~a one with `Id:`"
                             text (if (fsh-instance? e) "instance" "definition"))
                     (current-continuation-marks)))))
;; required-element-format : the message of an element an instance lacks (path, min, count, plural, rule)
(define required-element-format
  "`~a` is required (~a at least) and the instance gives it ~a time~a: add `* ~a = …`")
;; required-element-diagnostic? : any -> boolean   a diagnostic with that message, or of a value
;; that contradicts the profile (both what validating the example says again, at its rule)
(define (required-element-diagnostic? d)
  (and (diagnostic? d)
       (regexp-match? #rx"` is required [(][0-9]+ at least[)] and the instance gives it |, and its profile assigns .* there$" (diagnostic-message d))))
(define (compile-entity e [aliases (hash)] [profiles (hash)] [invariants (hash)])
  (check-id! e)
  (cond
    [(fsh-instance? e)
     (check-instance-of! e aliases profiles)
     (define conflicts (box '()))
     (define js (parameterize ([current-value-conflicts conflicts]) (compile-instance e aliases profiles)))
     ;; a value that contradicts what the InstanceOf assigns there (SUSHI: "Cannot assign X to this
     ;; element; a different Y is already assigned"), an error at the instance
     (when (current-rule-diagnostics)
       (for ([c (in-list (unbox conflicts))])
         (rule-diagnostic! e (entity-label e)
                           (format "the instance gives `~a` the value ~a, and its profile assigns ~a there"
                                   (car c) (jsexpr->string (cadr c)) (jsexpr->string (caddr c))))))
     ;; an element its InstanceOf requires that the instance does not give (SUSHI: "Element X has
     ;; minimum cardinality N but occurs K time(s)"), an error at the instance; it is still written
     (define of (resolve-alias (assoc-ref (fsh-instance-metadata e) 'instanceOf) aliases))
     (when (and (current-rule-diagnostics) (sd-index-available?) (string? of))
       (for ([m (in-list (missing-required js (profile-elements of)))])
         (rule-diagnostic! e (entity-label e)
                           (format required-element-format
                                   (car m) (cadr m) (caddr m) (if (= (caddr m) 1) "" "s")
                                   (regexp-replace #rx"^[^.]+[.]" (regexp-replace #rx"\\[x\\]$" (car m) "") "")))))
     js]
    [(fsh-valueset? e)   (compile-valueset e aliases)]
    [(fsh-codesystem? e) (compile-codesystem e aliases)]
    [(fsh-invariant? e)  (compile-invariant e)]
    [(or (fsh-profile? e) (fsh-extension? e)) (compile-profile e invariants)]
    [(or (fsh-logical? e) (fsh-resource? e)) (compile-logical-resource e)]
    [else #f]))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in fhir/diagnostic diagnostic-severity diagnostic->string))
  ;; parameter values: `\)` and `\,` are the characters; an argument is inserted verbatim
  (check-equal? (unescape-param "Aortic structure (body structure\\)") "Aortic structure (body structure)")
  (check-equal? (param-arg '(parameter "Canonical(TG263CS\\),")) "Canonical(TG263CS)")   ; the escaped paren survives
  (check-equal? (param-arg '(lastParameter " SCT)")) "SCT")
  (check-equal? (param-arg '(lastParameter "[[a, b)]] )")) "a, b)")
  (check-equal? (unescape-param "a\\, b") "a, b")
  (check-equal? (substitute "x = {d}" (substitution (list "d") (list "\"a (b\\) & c\""))) "x = \"a (b\\) & c\"")
  ;; an insert under a path prefixes every kind of rule the RuleSet holds — mapping and
  ;; content-reference rules too (compile's own prefixer used to leave them as written)
  (check-equal? (rule-mapping-path (prefix-rule "section" (rule-mapping "code" "Composition.code" #f #f))) "section.code")
  (check-equal? (rule-add-cr-element-path (prefix-rule "section" (rule-add-cr-element "sub" "0..*" '() "#x" #f #f))) "section.sub")
  (check-equal? (rule-card-path (prefix-rule "section" (rule-card #f "1..1" '()))) "section")
  ;; a parameterized RuleSet in an instance: the display argument with an escaped paren arrives whole
  (define cm (car (fsh->fhir (string-append
                              "RuleSet: MapConcept(code, display, targetcode, targetdisplay)\n"
                              "* group[=].element[+].code = {code}\n* group[=].element[=].display = {display}\n"
                              "* group[=].element[=].target.code = {targetcode}\n* group[=].element[=].target.display = {targetdisplay}\n\n"
                              "Instance: cm\nInstanceOf: ConceptMap\n* status = #draft\n* group[+].source = \"http://a\"\n* group[=].target = \"http://b\"\n"
                              "* insert MapConcept(#A_Aorta, \"Aorta\", #15825003, \"Aortic structure (body structure\\)\")\n"))))
  (define cm-target (car (hash-ref (car (hash-ref (car (hash-ref cm 'group)) 'element)) 'target)))
  (check-equal? (hash-ref cm-target 'display) "Aortic structure (body structure)")
  (check-equal? (hash-ref cm-target 'code) "15825003")
  ;; an insert under a path: the RuleSet's rules apply to that element (a path-less caret too)
  (define under-path (car (fsh->fhir (string-append
                                      "RuleSet: M\n* ^comment = \"mandatory\"\n* ^short = \"m\"\n\n"
                                      "Profile: P\nParent: Observation\n* code 1..1\n* code insert M\n"))))
  (define code-el (findf (lambda (e) (equal? (hash-ref e 'id) "Observation.code")) (hash-ref (hash-ref under-path 'differential) 'element)))
  (check-equal? (hash-ref code-el 'comment #f) "mandatory")
  (check-equal? (hash-ref code-el 'short #f) "m")
  ;; a problem in a rule a parameterized RuleSet makes is at the insert that made it — where the
  ;; argument is written — not at its place in the instantiated text (`<input>:2:5`, a file nobody has)
  (let-values ([(_out _errs ds)
                (compile-doc/diagnostics
                 (parse-fsh-doc "RuleSet: Req(path)\n* {path} 1..1 MS\n\nProfile: P\nParent: Patient\n* gender 1..1\n* insert Req(nmae)\n"
                                #:source "req.fsh"))])
    (check-equal? (map diagnostic->string ds)
                  '("req.fsh:7:3: fsh compile error: profile: P: `nmae` is not an element of Patient (did you mean `name`?)")))
  ;; a Mapping's rules may come from a RuleSet (its inserts were left unexpanded, the rules lost)
  (define mapped (car (fsh->fhir (string-append
                                  "RuleSet: NameMap\n* name -> \"PID-5\"\n\n"
                                  "Profile: MP\nParent: Patient\n* name 1..*\n\n"
                                  "Mapping: V2\nSource: MP\nTarget: \"HL7 V2\"\n* identifier -> \"PID-3\"\n* insert NameMap\n"))))
  (check-equal? (for/list ([e (in-list (hash-ref (hash-ref mapped 'differential) 'element))] #:when (hash-ref e 'mapping #f))
                  (list (hash-ref e 'id) (hash-ref (car (hash-ref e 'mapping)) 'map)))
                '(("Patient.name" "PID-5") ("Patient.identifier" "PID-3")))
  ;; a plain RuleSet whose rules insert a parameterized one: the nested insert expands too
  (define nested (car (fsh->fhir (string-append
                                  "RuleSet: NotUsed(path)\n* {path} ^short = \"Not used\"\n\n"
                                  "RuleSet: Common\n* subject 1..1\n* insert NotUsed(device)\n\n"
                                  "Profile: P\nParent: Observation\n* insert Common\n"))))
  (check-equal? (for/list ([e (in-list (hash-ref (hash-ref nested 'differential) 'element))]) (hash-ref e 'id))
                '("Observation.subject" "Observation.device"))
  ;; a parameterized RuleSet inserting another with its arguments (HRex's concept maps): the nested
  ;; reference re-texts with its paren, codes and [[…]] arguments intact
  (define nested2 (car (fsh->fhir (string-append
                                   "RuleSet: map(src, sdisp, tgt)\n* group[+].element[+].code = {src}\n* group[=].element[=].display = \"{sdisp}\"\n* group[=].element[=].target.code = {tgt}\n\n"
                                   "RuleSet: mapwide(src, sdisp, tgt)\n* insert map({src}, [[{sdisp}]], {tgt})\n* group[=].element[=].target.equivalence = #wider\n\n"
                                   "Instance: CM\nInstanceOf: ConceptMap\nUsage: #definition\n* status = #active\n* insert mapwide(#101Y00000X, [[Counselor (occupation)]], #224595007)\n"))))
  (define cm-el (car (hash-ref (car (hash-ref nested2 'group)) 'element)))
  (check-equal? (hash-ref cm-el 'code) "101Y00000X")
  (check-equal? (hash-ref cm-el 'display) "Counselor (occupation)")
  (check-equal? (hash-ref (car (hash-ref cm-el 'target)) 'equivalence) "wider")
  ;; an embedded resource typed by its resourceType: Coverage.class repeats, each block with its type
  (define emb (car (fsh->fhir (string-append
                               "Instance: PX\nInstanceOf: Parameters\nUsage: #example\n* parameter[0].name = \"cov\"\n* parameter[0].resource\n  * resourceType = \"Coverage\"\n  * status = #active\n"
                               "  * class[0]\n    * type = http://x#group\n    * value = \"A\"\n  * class[+]\n    * type = http://x#plan\n    * value = \"B\"\n"))))
  (define cov (hash-ref (car (hash-ref emb 'parameter)) 'resource))
  (check-equal? (map (lambda (c) (hash-ref c 'value)) (hash-ref cov 'class)) '("A" "B"))
  (check-equal? (map (lambda (c) (hash-ref (car (hash-ref (hash-ref c 'type) 'coding)) 'code)) (hash-ref cov 'class)) '("group" "plan"))
  ;; a Reference to a contained instance is local
  (define task (car (fsh->fhir (string-append
                                 "Instance: inner\nInstanceOf: Patient\nUsage: #inline\n* gender = #female\n\n"
                                 "Instance: t1\nInstanceOf: Task\nUsage: #example\n* status = #requested\n* intent = #order\n* contained[0] = inner\n* for = Reference(inner)\n"))))
  (check-equal? (hash-ref (hash-ref task 'for) 'reference) "#inner")
  ;; SUSHI's required values through a choice's type slice: the profile pins valueIdentifier.type on
  ;; a required slice, the instance writes only the value
  (define tslice (fsh->fhir (string-append
                             "Profile: MM\nParent: Parameters\n* parameter ^slicing.discriminator.type = #value\n* parameter ^slicing.discriminator.path = \"name\"\n* parameter ^slicing.rules = #open\n"
                             "* parameter contains MemberIdentifier 1..1\n* parameter[MemberIdentifier].name = \"MemberIdentifier\" (exactly)\n"
                             "* parameter[MemberIdentifier].valueIdentifier 1..\n* parameter[MemberIdentifier].valueIdentifier.type 1..\n* parameter[MemberIdentifier].valueIdentifier.type = http://x/cs#UMB\n\n"
                             "Instance: out\nInstanceOf: MM\nUsage: #example\n* parameter[MemberIdentifier].valueIdentifier.value = \"55678\"\n")))
  (define out-param (car (hash-ref (cadr tslice) 'parameter)))
  (check-equal? (hash-ref out-param 'name) "MemberIdentifier")
  (check-equal? (hash-ref (car (hash-ref (hash-ref (hash-ref out-param 'valueIdentifier) 'type) 'coding)) 'code) "UMB")
  (check-equal? (hash-ref (hash-ref out-param 'valueIdentifier) 'value) "55678")
  ;; and through an embedded resource the slice constrains to a profile: the profile's required
  ;; slice (category disclosure) is filled into the Consent the instance embeds
  (define embedded (fsh->fhir (string-append
                               "Profile: C\nParent: Consent\n* category ^slicing.discriminator.type = #value\n* category ^slicing.discriminator.path = \"$this\"\n* category ^slicing.rules = #open\n"
                               "* category contains disclosure 1..1\n* category[disclosure] = http://x/act#IDSCL\n\n"
                               "Profile: MM2\nParent: Parameters\n* parameter ^slicing.discriminator.type = #value\n* parameter ^slicing.discriminator.path = \"name\"\n* parameter ^slicing.rules = #open\n"
                               "* parameter contains Consent 1..1\n* parameter[Consent].name = \"Consent\" (exactly)\n* parameter[Consent].resource 1..\n* parameter[Consent].resource only C\n\n"
                               "Instance: in\nInstanceOf: MM2\nUsage: #example\n* parameter[Consent].resource\n  * resourceType = \"Consent\"\n  * status = #active\n  * scope = http://x/scope#patient-privacy\n")))
  (define consent (hash-ref (car (hash-ref (caddr embedded) 'parameter)) 'resource))
  (check-equal? (hash-ref consent 'status) "active")
  (check-equal? (hash-ref (car (hash-ref (car (hash-ref consent 'category)) 'coding)) 'code) "IDSCL")
  ;; a slice the embedded resource USES gets its profile's pattern (Coverage.class:group's type)
  (define emb2 (fsh->fhir (string-append
                           "Profile: CV\nParent: Coverage\n* class ^slicing.discriminator.type = #pattern\n* class ^slicing.discriminator.path = \"type\"\n* class ^slicing.rules = #open\n"
                           "* class contains group 0..1\n* class[group].type = http://x/cc#group\n\n"
                           "Profile: MM3\nParent: Parameters\n* parameter ^slicing.discriminator.type = #value\n* parameter ^slicing.discriminator.path = \"name\"\n* parameter ^slicing.rules = #open\n"
                           "* parameter contains Cov 1..1\n* parameter[Cov].name = \"Cov\" (exactly)\n* parameter[Cov].resource only CV\n\n"
                           "Instance: in2\nInstanceOf: MM3\nUsage: #example\n* parameter[Cov].resource\n  * resourceType = \"Coverage\"\n  * status = #active\n  * class[group].value = \"CB135\"\n  * class[+]\n    * type = http://x/cc#subplan\n    * value = \"P7\"\n")))
  ;; … and a bare `class[+]` after `class[group]` is element 0 (SUSHI's counters are per path WITH the
  ;; slice name), so it writes over the slice's element: the value replaced, the slice's pattern coding
  ;; ahead of the instance's own
  (define cov2 (hash-ref (car (hash-ref (caddr emb2) 'parameter)) 'resource))
  (check-equal? (map (lambda (c) (hash-ref c 'value)) (hash-ref cov2 'class)) '("P7"))
  (check-equal? (map (lambda (c) (hash-ref c 'code)) (hash-ref (hash-ref (car (hash-ref cov2 'class)) 'type) 'coding)) '("group" "subplan"))
  ;; a document: an alias resolves, a profile becomes a differential, an instance a resource
  (define out (fsh->fhir (string-append
                          "Alias: $sct = http://snomed.info/sct\n\n"
                          "Profile: BP\nParent: Observation\nTitle: \"Blood pressure\"\n* component 2..* MS\n* code = $sct#75367002\n\n"
                          "Instance: p1\nInstanceOf: Patient\nUsage: #example\n* gender = #female\n* name[0].family = \"Doe\"")))
  (check-equal? (map (lambda (r) (hash-ref r 'resourceType)) out) '("StructureDefinition" "Patient"))
  (define sd (car out))
  (check-equal? (hash-ref sd 'name) "BP")
  (check-equal? (hash-ref sd 'type) "Observation")
  (check-equal? (hash-ref sd 'derivation) "constraint")
  (define elements (hash-ref (hash-ref sd 'differential) 'element))
  (define component (findf (lambda (e) (equal? (hash-ref e 'path) "Observation.component")) elements))
  (check-equal? (hash-ref component 'min) 2)
  (check-equal? (hash-ref component 'mustSupport) #t)
  (define code (findf (lambda (e) (equal? (hash-ref e 'path) "Observation.code")) elements))
  (check-equal? (hash-ref (car (hash-ref (hash-ref code 'patternCodeableConcept) 'coding)) 'system) "http://snomed.info/sct")
  (define p (cadr out))
  (check-equal? (hash-ref p 'id) "p1")
  (check-equal? (hash-ref p 'gender) "female")
  (check-equal? (hash-ref (car (hash-ref p 'name)) 'family) "Doe")
  ;; compile-doc/errors answers every entity that compiles and the errors by entity label
  (define-values (ok errs)
    (compile-doc/errors (list (fsh-profile "P2" (list (cons 'parent "Patient")) (list (rule-card "name" "1..*" '())))
                              (fsh-instance "p2" (list (cons 'instanceOf "Patient")) '()))))
  (check-equal? (map (lambda (r) (hash-ref r 'resourceType)) ok) '("StructureDefinition" "Patient"))
  (check-equal? errs '())
  (check-equal? (entity-label (fsh-profile "P2" '() '())) "profile: P2")
  ;; what an agent misspells is said where it is written, with the nearest name: an element, an
  ;; InstanceOf (an error), a value set nothing defines (a warning: the profile still compiles)
  (define-values (_ok2 errs2 ds2)
    (compile-doc/diagnostics
     (parse-fsh-doc (string-append "Profile: P3\nParent: Observation\n* stauts 1..1\n* code from NoSuchVS (required)\n\n"
                                   "Instance: x\nInstanceOf: Observaton\n* status = #final\n"))))
  (check-equal? (map car errs2) '("instance: x"))
  (check-regexp-match #rx"InstanceOf `Observaton` not found.*did you mean `Observation`" (exn-message (cdar errs2)))
  (check-equal? (sort (map (lambda (d) (list (diagnostic-severity d) (diagnostic-message d))) ds2) string<? #:key cadr)
                '((error "profile: P3: `stauts` is not an element of Observation (did you mean `status`?)")
                  (error "profile: P3: value set `NoSuchVS` not found: it is not a ValueSet of this project or its packages, an alias, or a url"))))
