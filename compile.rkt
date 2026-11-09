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
         racket/contract
         (only-in racket/list append-map last filter-map remove-duplicates)
         "parser/ast.rkt"
         "parser/path.rkt"
         (only-in "parser/parse.rkt" parse-fsh-doc)
         (only-in fhir/diagnostic diagnostic attached-loc)
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
(define (fsh->fhir str) (compile-doc (parse-fsh-doc str)))
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
          [(rule-binding _ (? string? vs) _)
           (unless (regexp-match? #rx":" (valueset-url vs))
             (rule-diagnostic! r (entity-label e)
                               (format "value set `~a` not found: it is not a ValueSet of this project or its packages, an alias, or a url" vs)
                               #:severity 'warning))]
          [_ (void)])))
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
;; base-element-names : type -> (listof string)|#f   its top-level element names ("value[x]", "name"),
;; per package context (every entity of a type asks; the snapshot scan was repeated for each)
(define base-element-name-cache (make-hash))
(define (base-element-names type)
  (hash-ref! base-element-name-cache (cons (and (sd-index-available?) (sd-context)) type)
             (lambda () (scan-base-element-names type))))
(define (scan-base-element-names type)
  (define sd (and (sd-index-available?) (resolve-sd-raw type)))
  (define els (and (hash? sd) (hash-ref (hash-ref sd 'snapshot (hash)) 'element #f)))
  (and (list? els)
       (for*/list ([el (in-list els)]
                   [m (in-value (regexp-match (pregexp (string-append "^" (regexp-quote type) "[.]([^.:]+)$")) (hash-ref el 'path "")))]
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
(define (compile-doc/errors entities)
  ;; one document's RuleSet instantiations are parsed once per distinct text (see instantiate-ruleset)
  (parameterize ([current-instantiations (make-hash)])
    (compile-doc/errors* entities)))
(define (compile-doc/errors* entities)
  (define errors '())
  (define aliases  (for/hash ([e (in-list entities)] #:when (fsh-alias? e))
                     (values (fsh-alias-name e) (fsh-alias-value e))))
  (define rulesets (for/hash ([e (in-list entities)] #:when (fsh-ruleset? e))
                     (values (fsh-ruleset-name e) (fsh-ruleset-rules e))))
  ;; parameterized RuleSets: name -> (cons params body-tokens)
  (define param-rulesets (for/hash ([e (in-list entities)] #:when (fsh-param-ruleset? e))
                           (values (fsh-param-ruleset-name e)
                                   (cons (fsh-param-ruleset-params e) (fsh-param-ruleset-body e)))))
  ;; profile name -> (cons parent-base profile-id) — for Instance base resolution + meta.profile
  ;; keyed by NAME, ID and canonical URL — SUSHI resolves InstanceOf/Parent by any of the three
  (define profiles (for*/hash ([e (in-list entities)]
                               #:when (or (fsh-profile? e) (fsh-extension? e))
                               [id (in-value (or (assoc-ref (entity-meta e) 'id) (entity-name e)))]
                               [key (in-list (remove-duplicates
                                              (list (entity-name e) id
                                                    (string-append (canonical-base) "/StructureDefinition/" id))))])
                     (values key
                             (cons (or (assoc-ref (entity-meta e) 'parent)
                                       (if (fsh-extension? e) "Extension" "DomainResource"))
                                   id))))
  ;; an Invariant's fields come from its keywords (`Severity:`) OR from assignment rules
  ;; (`* severity = #error`, `* expression = "…"`) — both forms are FSH
  (define invariants (for/hash ([e (in-list entities)] #:when (fsh-invariant? e))
                       (define m (fsh-invariant-metadata e))
                       (define (rule-val k)                        ; an Invariant's rules may come from a RuleSet
                         (for/or ([r (in-list (expand-rules (fsh-invariant-rules e) rulesets param-rulesets))])
                           (match r [(rule-assign (== k) v _) (match v [(struct fsh-code (c _)) c] [_ v])] [_ #f])))
                       (define (field k [rk (symbol->string k)]) (or (assoc-ref m k) (rule-val rk)))
                       (values (fsh-invariant-name e)
                               (drop-false
                                (hasheq 'severity (strip# (or (field 'severity) "#error"))
                                        'human (or (field 'description) (rule-val "human"))
                                        'expression (field 'expression)
                                        'xpath (field 'xpath))))))
  ;; Instances by name and id, compiled on demand (memoised, cycle-guarded) so one instance can be
  ;; embedded in another; `Usage: #inline` instances are ONLY embedded, never emitted on their own.
  ;; names take precedence over ids (an id may be shared by instances of different types)
  (define instances
    (let ([by-id (for/fold ([h (hash)]) ([e (in-list entities)] #:when (fsh-instance? e))
                   (if (hash-has-key? h (instance-id e)) h (hash-set h (instance-id e) e)))]   ; first wins
          [by-name (for/hash ([e (in-list entities)] #:when (fsh-instance? e)) (values (fsh-instance-name e) e))])
      (for/fold ([h by-id]) ([(k v) (in-hash by-name)]) (hash-set h k v))))
  (define memo (make-hash))
  (define (resolve-instance name)
    (define e (hash-ref instances name #f))
    (cond [(not e) #f]
          [(hash-has-key? memo e) (hash-ref memo e)]          ; #f while in progress: a cycle stays a name
          [else (hash-set! memo e #f)
                (define js (with-handlers ([exn:fail? (lambda (_) #f)])
                             (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants)))
                (hash-set! memo e js)
                js]))
  ;; logical models and custom resources by name, id and url -> their canonical url
  (define logicals (for*/hash ([e (in-list entities)] #:when (or (fsh-logical? e) (fsh-resource? e))
                               [name (in-value (if (fsh-logical? e) (fsh-logical-name e) (fsh-resource-name e)))]
                               [id (in-value (or (assoc-ref (if (fsh-logical? e) (fsh-logical-metadata e) (fsh-resource-metadata e)) 'id) name))]
                               [url (in-value (string-append (canonical-base) "/StructureDefinition/" id))]
                               [key (in-list (remove-duplicates (list name id url)))])
                     (values key url)))
  (define (inline? e) (and (fsh-instance? e) (equal? (assoc-ref (fsh-instance-metadata e) 'usage) "#inline")))
  ;; this document's logical models and custom resources are registered as types (fhir/sd/logical,
  ;; the same registry the FHIR towers fill), so an Instance of one builds like any resource
  (for ([e (in-list entities)] #:when (or (fsh-logical? e) (fsh-resource? e)))
    (define sd (with-handlers ([exn:fail? (lambda (_) #f)])
                 (parameterize ([current-aliases aliases] [current-logicals logicals])
                   (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants))))
    (when (hash? sd) (register-structure-definition! sd)))
  ;; this document's Profiles (and logical models) compiled on demand (name/id/url ->
  ;; StructureDefinition jsexpr), for the required values an Instance inherits from its InstanceOf
  ;; profile and the characteristics a logical model inherits from its Parent
  (define profile-entities
    (for*/hash ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e))
                [id (in-value (or (assoc-ref (entity-meta e) 'id) (entity-name e)))]
                [k (in-list (remove-duplicates (list (entity-name e) id (string-append (canonical-base) "/StructureDefinition/" id))))])
      (values k e)))
  (define profile-memo (make-hash))
  (define (resolve-profile-sd key)
    (define e (hash-ref profile-entities key #f))
    (cond [(not e) #f]
          [(hash-has-key? profile-memo e) (hash-ref profile-memo e)]
          [else (hash-set! profile-memo e #f)
                (define sd (with-handlers ([exn:fail? (lambda (_) #f)])
                             (parameterize ([emit-snapshot? #f])
                               (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants))))
                (hash-set! profile-memo e sd)
                sd]))
  ;; profile name/id -> (hash element-path -> REQUIRED slice names in declaration order), parents
  ;; first. SUSHI creates a profile's required slices before applying an instance's rules (in profile
  ;; order); every other slice takes its place in the order the instance first mentions it.
  (define own-slice-orders
    (for*/hash ([e (in-list entities)] #:when (or (fsh-profile? e) (fsh-extension? e))
                [k (in-list (remove-duplicates (list (entity-name e) (or (assoc-ref (entity-meta e) 'id) (entity-name e)))))])
      (values k (for/fold ([h (hash)]) ([r (in-list (entity-rules e))] #:when (rule-contains? r))
                  (hash-update h (rule-contains-path r)
                               (lambda (l) (append l (for/list ([it (in-list (rule-contains-items r))]
                                                                #:when (let ([c (contains-item-card it)]) (and (string? c) (>= (card-lo c) 1))))
                                                       (or (contains-item-named-as it) (contains-item-name it)))))
                               '())))))
  (define (slice-orders-of name [fuel 16])
    (define own (hash-ref own-slice-orders name (hash)))
    (define parent (let ([p (hash-ref profiles name #f)]) (and p (car p))))
    (define inherited (if (and parent (> fuel 0) (hash-ref own-slice-orders parent #f)) (slice-orders-of parent (sub1 fuel)) (hash)))
    (for/fold ([h inherited]) ([(k v) (in-hash own)]) (hash-update h k (lambda (l) (append l v)) '())))
  ;; this document's ValueSets / CodeSystems: name and id -> canonical URL
  (define local-canonicals
    (for*/hash ([e (in-list entities)] #:when (or (fsh-valueset? e) (fsh-codesystem? e))
                [kind (in-value (if (fsh-valueset? e) "ValueSet" "CodeSystem"))]
                [name (in-value (if (fsh-valueset? e) (fsh-valueset-name e) (fsh-codesystem-name e)))]
                [id (in-value (or (assoc-ref (if (fsh-valueset? e) (fsh-valueset-metadata e) (fsh-codesystem-metadata e)) 'id) name))]
                [k (in-list (remove-duplicates (list name id)))])
      (values k (string-append (canonical-base) "/" kind "/" id))))
  ;; instance name/id -> "Type/id", from metadata alone (no compilation, so references never cycle)
  (define (instance-ref name)
    (define e (hash-ref instances name #f))
    (and e (let* ([m (fsh-instance-metadata e)]
                  [of (let ([o (assoc-ref m 'instanceOf)]) (and o (resolve-alias o aliases)))]
                  [base (and of (instance-base of profiles))])
             (and base (string-append base "/" (instance-id e))))))
  ;; Invariants are consumed only as SD constraint sources (like SUSHI) — not emitted standalone.
  (define compiled
    (parameterize ([current-instance-resolver resolve-instance] [current-instance-ref instance-ref]
                   [current-profile-sd resolve-profile-sd]
                   [current-aliases aliases] [current-profiles profiles] [current-logicals logicals]
                   [current-slice-orders slice-orders-of]
                   [current-local-canonical (lambda (n) (hash-ref local-canonicals n #f))])
    (for*/list ([e (in-list entities)]
                #:unless (or (fsh-alias? e) (fsh-ruleset? e) (fsh-param-ruleset? e)
                             (fsh-invariant? e) (fsh-mapping? e))
                ;; a RuleSet that fails to expand (its instantiated body does not parse) fails this
                ;; entity alone, like any other compile error
                [x (in-value (with-handlers ([exn:fail? (lambda (x) (set! errors (cons (cons (entity-label e) x) errors)) #f)])
                               (expand-entity-inserts e rulesets param-rulesets)))]
                #:when x
                [_ (in-value (check-element-paths! x))]          ; rule-level diagnostics (accumulated)
                #:unless (inline? e)
                [out (in-value
                      (with-handlers ([exn:fail? (lambda (x) (set! errors (cons (cons (entity-label e) x) errors)) #f)])
                        (compile-entity x aliases profiles invariants)))]
                #:when out)
      out)))
  ;; Mapping entities inject into their Source StructureDefinition (SUSHI-style) — a post-pass.
  (values (for/fold ([sds compiled]) ([m (in-list entities)] #:when (fsh-mapping? m))
            (apply-mapping sds m))
          (reverse errors)))
;; entity-label : entity -> string   "Profile: X" etc., for error reports
(define (entity-label e)
  (define v (struct->vector e))
  (format "~a: ~a" (regexp-replace #rx"^struct:fsh-" (symbol->string (vector-ref v 0)) "")
          (if (> (vector-length v) 1) (vector-ref v 1) "?")))
(define (entity-name e)
  (cond [(fsh-profile? e) (fsh-profile-name e)] [(fsh-extension? e) (fsh-extension-name e)]
        [(fsh-logical? e) (fsh-logical-name e)] [(fsh-resource? e) (fsh-resource-name e)] [else #f]))
(define (entity-meta e)
  (cond [(fsh-profile? e) (fsh-profile-metadata e)] [(fsh-extension? e) (fsh-extension-metadata e)]
        [(fsh-logical? e) (fsh-logical-metadata e)] [(fsh-resource? e) (fsh-resource-metadata e)] [else '()]))
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
           (under (rule-insert-path r) (expand-rules (instantiate-ruleset (car pr) (cdr pr) args) rulesets param-rulesets))])]
       [else (list r)]))
   rules))
;; under : path|#f rules -> rules   `* sectionProblems insert M` (or an insert indented under a rule)
;; applies the RuleSet's rules under that path: a rule's path is joined to it, a path-less one (an
;; SD-level caret) becomes a rule on the element itself
(define (under prefix rules)
  (if (and (string? prefix) (not (member prefix '("" "."))))
      (map (lambda (r) (prefix-rule prefix r)) rules)
      rules))
(define (prefix-rule prefix r)
  (map-rule-path (lambda (p) (if (or (not p) (equal? p "") (equal? p ".")) prefix (string-append prefix "." p))) r))
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
;; instantiate-ruleset : params body-tokens args -> rules
;; a parameterized RuleSet instantiated: {param}->arg substituted in the raw body, re-parsed to rules.
;; The rules depend only on the substituted text, so within one document (current-instantiations) a
;; text is parsed once however many inserts produce it (mCODE: thousands of inserts, 30% of its time).
(define current-instantiations (make-parameter #f))
(define (instantiate-ruleset params body args)
  (define subst (substitution params args))
  ;; a STAR token carries its own leading "\n* " (starts a rule line); other tokens get a space.
  ;; padding a STAR would let the WS lexer eat the newline the STAR rule needs.
  (define text (apply string-append "RuleSet: __expand"
                      (for/list ([t (in-list body)])
                        (define raw (let ([x (cdr t)]) (if (code-tok? x) (code-tok-text x) x)))
                        (define v (requote (car t) (substitute raw subst)))
                        (if (eq? (car t) 'STAR) v (string-append " " v)))))
  (define (parse)
    (define ds (parse-fsh-doc text))
    (if (and (pair? ds) (fsh-ruleset? (car ds))) (fsh-ruleset-rules (car ds)) '()))
  (define memo (current-instantiations))
  (if memo (hash-ref! memo text parse) (parse)))
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
(define (expand-entity-inserts e rulesets param-rulesets)
  (define (xr rs) (parameterize ([current-insert-label (entity-label e)]) (expand-rules rs rulesets param-rulesets)))
  (match e
    [(fsh-profile n m rs)    (fsh-profile n m (xr rs))]
    [(fsh-extension n m rs)  (fsh-extension n m (xr rs))]
    [(fsh-instance n m rs)   (fsh-instance n m (xr rs))]
    [(fsh-valueset n m rs)   (fsh-valueset n m (xr rs))]
    [(fsh-codesystem n m rs) (fsh-codesystem n m (xr rs))]
    [(fsh-logical n m rs)    (fsh-logical n m (xr rs))]
    [(fsh-resource n m rs)   (fsh-resource n m (xr rs))]
    [_ e]))
;; ---- entity dispatch -----------------------------------------------------------------------
(define (compile-entity e [aliases (hash)] [profiles (hash)] [invariants (hash)])
  (cond
    [(fsh-instance? e)   (check-instance-of! e aliases profiles) (compile-instance e aliases profiles)]
    [(fsh-valueset? e)   (compile-valueset e aliases)]
    [(fsh-codesystem? e) (compile-codesystem e aliases)]
    [(fsh-invariant? e)  (compile-invariant e)]
    [(or (fsh-profile? e) (fsh-extension? e)) (compile-profile e invariants)]
    [(or (fsh-logical? e) (fsh-resource? e)) (compile-logical-resource e)]
    [else #f]))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in fhir/diagnostic diagnostic-severity diagnostic-message))
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
                  (warning "profile: P3: value set `NoSuchVS` not found: it is not a ValueSet of this project or its packages, an alias, or a url"))))
