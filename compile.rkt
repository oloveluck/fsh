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

(require racket/match
         racket/string
         racket/contract
         (only-in racket/list append-map make-list last filter-map remove-duplicates)
         "parser/ast.rkt"
         "parser/path.rkt"
         "caret.rkt"
         (only-in "parser/parse.rkt" parse-fsh-doc)
         (only-in "resolve.rkt" element-fhir-type field-info)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/model/schema ftype-tag ftype-type-name)
         (only-in fhir/serialize/jsexpr-util drop-false)
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhir/sd/snapshot generate-snapshot)
         (only-in fhir/sd/sd-index resolve-sd-raw resolve-canonical-raw sd-index-available?))

(provide (contract-out
          [canonical-base (parameter/c string?)]
          [emit-snapshot? (parameter/c boolean?)]
          ;; the FHIR version stamped on generated StructureDefinitions/IGs ("5.0.0" by default; set
          ;; "4.0.1" — and register the R4 tower — to compile an R4 IG)
          [fsh-fhir-version (parameter/c string?)]
          [compile-entity (->* (struct?) (hash? hash? hash?) (or/c hash? #f))]
          [compile-doc    (-> (listof struct?) (listof hash?))]
          [compile-doc/errors (-> (listof struct?) (values (listof hash?) (listof (cons/c string? exn?))))]
          [fsh->fhir      (-> string? (listof hash?))]))

;; the IG canonical URL base (SUSHI default in the golden corpus is http://example.org)
(define canonical-base (make-parameter "http://example.org"))
(define fsh-fhir-version (make-parameter "5.0.0"))
;; opt-in: also emit a full snapshot (differential merged onto the base's snapshot). Off by default
;; (the golden corpus is differential-only); loads the R5 core package on first use.
(define emit-snapshot? (make-parameter #f))

;; attach a snapshot to a built StructureDefinition when emit-snapshot? is on
(define (maybe-snapshot sd)
  (if (emit-snapshot?)
      (let ([snap (generate-snapshot sd)]) (if (pair? snap) (hash-set sd 'snapshot (hasheq 'element snap)) sd))
      sd))

;; FSH source -> (listof FHIR jsexpr)
(define (fsh->fhir str) (compile-doc (parse-fsh-doc str)))

;; compile a whole document: build alias/ruleset/profile tables, expand inserts, compile each entity
;; compile-doc : entities -> (listof jsexpr)   raises on the first entity that fails to compile
(define (compile-doc entities)
  (define-values (out errors) (compile-doc/errors entities))
  (when (pair? errors) (raise (cdr (car errors))))
  out)

;; compile-doc/errors : entities -> (values (listof jsexpr) (listof (cons entity-label exn)))
;; Like SUSHI, a failing entity is reported and the rest of the document still compiles.
(define (compile-doc/errors entities)
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
  (define invariants (for/hash ([e (in-list entities)] #:when (fsh-invariant? e))
                       (define m (fsh-invariant-metadata e))
                       (values (fsh-invariant-name e)
                               (hasheq 'severity (strip# (or (assoc-ref m 'severity) "#error"))
                                       'human (assoc-ref m 'description)
                                       'expression (assoc-ref m 'expression)))))
  ;; Instances by name and id, compiled on demand (memoised, cycle-guarded) so one instance can be
  ;; embedded in another; `Usage: #inline` instances are ONLY embedded, never emitted on their own.
  ;; an instance's id: `Id:`, else an `* id = "…"` rule, else its name
  (define (instance-id e)
    (or (assoc-ref (fsh-instance-metadata e) 'id)
        (for/or ([r (in-list (fsh-instance-rules e))])
          (match r [(rule-assign "id" (? string? v) _) v] [_ #f]))
        (fsh-instance-name e)))
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
  (define (inline? e) (and (fsh-instance? e) (equal? (assoc-ref (fsh-instance-metadata e) 'usage) "#inline")))
  ;; profile name/id -> (hash element-path -> slice names in declaration order), parents first
  (define own-slice-orders
    (for*/hash ([e (in-list entities)] #:when (fsh-profile? e)
                [k (in-list (remove-duplicates (list (fsh-profile-name e) (or (assoc-ref (fsh-profile-metadata e) 'id) (fsh-profile-name e)))))])
      (values k (for/fold ([h (hash)]) ([r (in-list (fsh-profile-rules e))] #:when (rule-contains? r))
                  (hash-update h (rule-contains-path r)
                               (lambda (l) (append l (for/list ([it (in-list (rule-contains-items r))])
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
                  [of (assoc-ref m 'instanceOf)]
                  [base (and of (instance-base of profiles))])
             (and base (string-append base "/" (instance-id e))))))
  ;; Invariants are consumed only as SD constraint sources (like SUSHI) — not emitted standalone.
  (define compiled
    (parameterize ([current-instance-resolver resolve-instance] [current-instance-ref instance-ref]
                   [current-aliases aliases] [current-profiles profiles]
                   [current-slice-orders slice-orders-of]
                   [current-local-canonical (lambda (n) (hash-ref local-canonicals n #f))])
    (for*/list ([e (in-list entities)]
                #:unless (or (fsh-alias? e) (fsh-ruleset? e) (fsh-param-ruleset? e)
                             (fsh-invariant? e) (fsh-mapping? e) (inline? e))
                [out (in-value
                      (with-handlers ([exn:fail? (lambda (x) (set! errors (cons (cons (entity-label e) x) errors)) #f)])
                        (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants)))]
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
  (cond [(fsh-profile? e) (fsh-profile-name e)] [(fsh-extension? e) (fsh-extension-name e)] [else #f]))
(define (entity-meta e)
  (cond [(fsh-profile? e) (fsh-profile-metadata e)] [(fsh-extension? e) (fsh-extension-metadata e)] [else '()]))

;; ---- insert / RuleSet expansion (plain + parameterized) ------------------------------------
(define (expand-rules rules rulesets param-rulesets)
  (append-map
   (lambda (r)
     (cond
       ;; plain `insert RuleSet` — ref is the ruleset name string
       [(and (rule-insert? r) (string? (rule-insert-ref r)))
        (define rs (hash-ref rulesets (rule-insert-ref r) #f))
        (if rs (expand-rules rs rulesets param-rulesets) (list r))]
       ;; parameterized `insert RuleSet(args)` — ref is (name param-token …)
       [(and (rule-insert? r) (pair? (rule-insert-ref r)))
        (define ref (rule-insert-ref r))
        (define pr (hash-ref param-rulesets (car ref) #f))
        (cond
          [(not pr) (list r)]
          [else
           (define args (filter-map param-arg (cdr ref)))
           (expand-rules (instantiate-ruleset (car pr) (cdr pr) args) rulesets param-rulesets)])]
       [else (list r)]))
   rules))

;; a (parameter|lastParameter "v,") token -> its cleaned argument value
(define (param-arg x)
  (match x
    [(list (or 'parameter 'lastParameter) v)
     (string-trim (regexp-replace #px"[),\\]]+$" (regexp-replace #px"^\\[+" (string-trim v) "") ""))]
    [_ #f]))

;; instantiate a parameterized RuleSet: substitute {param}->arg in the raw body, re-parse to rules
(define (instantiate-ruleset params body args)
  (define subst (map cons params args))
  ;; a STAR token carries its own leading "\n* " (starts a rule line); other tokens get a space.
  ;; padding a STAR would let the WS lexer eat the newline the STAR rule needs.
  (define text (apply string-append "RuleSet: __expand"
                      (for/list ([t (in-list body)])
                        (define v (substitute (cdr t) subst))
                        (if (eq? (car t) 'STAR) v (string-append " " v)))))
  (define ds (parse-fsh-doc text))
  (if (and (pair? ds) (fsh-ruleset? (car ds))) (fsh-ruleset-rules (car ds)) '()))

(define (substitute s subst)
  (for/fold ([s s]) ([kv (in-list subst)])
    (regexp-replace* (regexp (regexp-quote (string-append "{" (car kv) "}"))) s (cdr kv))))

(define (expand-entity-inserts e rulesets param-rulesets)
  (define (xr rs) (expand-rules rs rulesets param-rulesets))
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
    [(fsh-instance? e)   (compile-instance e aliases profiles)]
    [(fsh-valueset? e)   (compile-valueset e aliases)]
    [(fsh-codesystem? e) (compile-codesystem e aliases)]
    [(fsh-invariant? e)  (compile-invariant e)]
    [(or (fsh-profile? e) (fsh-extension? e)) (compile-profile e invariants)]
    [(or (fsh-logical? e) (fsh-resource? e)) (compile-logical-resource e)]
    [else #f]))

;; ---- Profile / Extension -> StructureDefinition.differential -------------------------------
(define (compile-profile e [invariants (hash)])
  (define-values (name meta rules)
    (match e [(fsh-profile n m r) (values n m r)] [(fsh-extension n m r) (values n m r)]))
  (define ext? (fsh-extension? e))
  (define parent (resolve-alias (or (assoc-ref meta 'parent) (if ext? "Extension" "DomainResource")) (current-aliases)))
  ;; a Parent defined in THIS document (by name, id or url): its canonical is local, and the
  ;; constrained type is found by following the Parent chain down to a base resource type
  (define local-parent (hash-ref (current-profiles) parent #f))
  (define base-type (if ext? "Extension" (instance-base parent (current-profiles))))
  (define parent-url (if local-parent
                         (string-append (canonical-base) "/StructureDefinition/" (cdr local-parent))
                         (sd-url parent)))
  (define id (or (assoc-ref meta 'id) name))
  (define self-url (string-append (canonical-base) "/StructureDefinition/" id))
  (define-values (elems0 order0) (fold-sd-rules base-type rules invariants self-url))
  (define-values (elems order)
    (if ext? (add-extension-scaffold elems0 order0 self-url meta) (values elems0 order0)))
  (define diff (for/list ([k (in-list (reverse order))]) (hash-ref elems k)))
  (define base (drop-false
                (hasheq 'resourceType "StructureDefinition"
                        'id id
                        'url self-url
                        'version "0.1.0"
                        'name name
                        'title (assoc-ref meta 'title)
                        'description (assoc-ref meta 'description)
                        'status "active"
                        'fhirVersion (fsh-fhir-version)
                        'kind (if ext? "complex-type" "resource")
                        'type base-type
                        'baseDefinition parent-url
                        'derivation "constraint"
                        ;; SUSHI carries the base definition's mapping declarations into the profile
                        'mapping (inherited-mappings base-type)
                        'differential (and (pair? diff) (hasheq 'element diff)))))
  (maybe-snapshot (apply-carets (hash-set (if ext? (add-extension-header base) base) 'abstract #f) rules)))

;; ---- Logical / Resource -> StructureDefinition (specialization, addElement) ----------------
(define (compile-logical-resource e)
  (define logical? (fsh-logical? e))
  (define-values (name meta rules)
    (if logical? (values (fsh-logical-name e) (fsh-logical-metadata e) (fsh-logical-rules e))
        (values (fsh-resource-name e) (fsh-resource-metadata e) (fsh-resource-rules e))))
  (define parent (or (assoc-ref meta 'parent) (if logical? "Base" "DomainResource")))
  (define id (or (assoc-ref meta 'id) name))
  (define self-url (string-append (canonical-base) "/StructureDefinition/" id))
  ;; the element paths of a logical/custom resource root at the entity id, not a FHIR type
  (define root (drop-false (hasheq 'id id 'path id
                                   'short (assoc-ref meta 'title) 'definition (assoc-ref meta 'description))))
  (define-values (elems order) (fold-sd-rules id rules (hash) self-url))
  (define diff (cons root (for/list ([k (in-list (reverse order))]) (hash-ref elems k))))
  (maybe-snapshot
   (apply-carets
    (hash-set
     (drop-false
      (hasheq 'resourceType "StructureDefinition" 'id id 'url self-url 'version "0.1.0" 'name name
              'title (assoc-ref meta 'title) 'description (assoc-ref meta 'description)
              'status "active" 'fhirVersion (fsh-fhir-version)
              'kind (if logical? "logical" "resource")
              'type (if logical? self-url id)
              'baseDefinition (sd-url parent) 'derivation "specialization"
              'differential (hasheq 'element diff)))
     'abstract #f)
    rules)))

;; fold SD rules into an id-keyed differential-element accumulator (shared by profile/ext/logical)
(define (fold-sd-rules base-type rules invariants self-url)
  (define (constraint-of key)
    (define inv (hash-ref invariants key #f))
    (drop-false (hasheq 'key key
                        'severity (and inv (hash-ref inv 'severity #f))
                        'human (and inv (hash-ref inv 'human #f))
                        'expression (and inv (hash-ref inv 'expression #f))
                        'source self-url)))
  (define element-counters (make-hash))      ; element id -> soft-index counters for its carets
  (for/fold ([elems (hash)] [order '()]) ([r (in-list rules)])
    (define p (rule-path-of r))
    (define-values (rid rpath rslice)
      (if (string? p) (element-loc base-type p) (values #f #f #f)))
    (match r
      [(rule-card p card flags) (upd elems order rid rpath rslice (lambda (h) (apply-card (apply-flags h flags) card)))]
      [(rule-flag p _ flags)    (upd elems order rid rpath rslice (lambda (h) (apply-flags h flags)))]
      [(rule-binding p vs str)  (upd elems order rid rpath rslice (lambda (h) (hash-set h 'binding (drop-false (hasheq 'strength (and str (symbol->string str)) 'valueSet (and vs (valueset-url vs)))))))]
      [(rule-only p types)      (upd elems order rid rpath rslice (lambda (h) (hash-set h 'type (map type->jsexpr types))))]
      [(rule-assign p v exact?) (upd elems order rid rpath rslice (lambda (h) (apply-pattern h base-type p v exact?)))]
      [(rule-caret p cp v)      (if p (upd elems order rid rpath rslice
                                           (lambda (h) (typed-caret-set h cp v 'ElementDefinition
                                                                        (hash-ref! element-counters rid make-hash))))
                                    (values elems order))]
      [(rule-obeys p invs)      (let-values ([(oid opath oslice) (if p (values rid rpath rslice) (values base-type base-type #f))])
                                  (upd elems order oid opath oslice (lambda (h) (hash-set h 'constraint (map constraint-of invs)))))]
      [(rule-add-element p card flags types short def)
       (upd elems order rid rpath rslice
            (lambda (h) (hash-set* (apply-card (apply-flags h flags) card)
                                   'short short 'definition (or def short)
                                   'type (and (pair? types) (map type->jsexpr types)))))]
      [(rule-contains p items)
       (define slice-min (for/sum ([it (in-list items)]) (card-lo (contains-item-card it))))
       (define-values (e1 o1) (upd elems order rid rpath rslice
                                   (lambda (h) (if (> slice-min 0) (hash-set h 'min slice-min) h))))
       (for/fold ([elems e1] [order o1]) ([it (in-list items)])
         (define sn (or (contains-item-named-as it) (contains-item-name it)))
         ;; reslicing: when the contains target is itself a slice, sub-slice ids join with "/"
         (define sub-id (if rslice (string-append rid "/" sn) (string-append rid ":" sn)))
         (define sub-name (if rslice (string-append rslice "/" sn) sn))
         (upd elems order sub-id rpath sub-name
              (lambda (h) (apply-card (apply-flags h (contains-item-flags it)) (contains-item-card it)))))]
      [_ (values elems order)])))

;; set only the keys whose value is non-#f (a drop-false-aware multi hash-set)
(define (hash-set* h . kvs)
  (let loop ([h h] [kvs kvs])
    (cond [(null? kvs) h]
          [(cadr kvs) (loop (hash-set h (car kvs) (cadr kvs)) (cddr kvs))]
          [else (loop h (cddr kvs))])))

;; base-type + FSH path -> (values ElementDefinition-id path sliceName). [x]->[x]; the first slice
;; bracket on a step is ":slice", further ones (reslicing) are "/subslice" (id "component:Lab/Chem").
(define (id-seg s)
  (let loop ([bs (fsh-step-brackets s)] [seen-slice? #f] [acc (fsh-step-name s)])
    (cond
      [(null? bs) acc]
      [else (define b (car bs))
            (case (fsh-bracket-kind b)
              [(choice) (loop (cdr bs) seen-slice? (string-append acc "[x]"))]
              [(slice)  (loop (cdr bs) #t (string-append acc (if seen-slice? "/" ":") (fsh-bracket-value b)))]
              [else (loop (cdr bs) seen-slice? acc)])])))
(define (path-seg s)
  (string-append (fsh-step-name s)
                 (if (for/or ([b (in-list (fsh-step-brackets s))]) (eq? (fsh-bracket-kind b) 'choice)) "[x]" "")))
;; the sliceName of a step = its slice brackets joined by "/" (Lab/Chem for a reslice), or #f
(define (step-slice s)
  (define slices (for/list ([b (in-list (fsh-step-brackets s))] #:when (eq? (fsh-bracket-kind b) 'slice))
                   (fsh-bracket-value b)))
  (and (pair? slices) (string-join slices "/")))

(define (element-loc base-type fsh-path-str)
  (define steps (if (equal? (string-trim fsh-path-str) ".")      ; `* . ^short = …` is the root element
                    '()
                    (fsh-path-steps (parse-fsh-path fsh-path-str))))
  (cond
    [(null? steps) (values base-type base-type #f)]
    [else
     (define id (string-append base-type "." (string-join (map id-seg steps) ".")))
     (define path (string-append base-type "." (string-join (map path-seg steps) ".")))
     (values id path (step-slice (last steps)))]))

;; the path field of any SD rule (or #f for path-less SD-level caret / root obeys)
(define (rule-path-of r)
  (match r
    [(rule-card p _ _) p] [(rule-flag p _ _) p] [(rule-binding p _ _) p] [(rule-only p _) p]
    [(rule-assign p _ _) p] [(rule-caret p _ _) p] [(rule-obeys p _) p] [(rule-contains p _) p]
    [(rule-add-element p _ _ _ _ _) p] [_ #f]))

(define (card-lo card)
  (match (regexp-match #px"^([0-9]*)\\.\\." card) [(list _ lo) (if (positive? (string-length lo)) (string->number lo) 0)] [_ 0]))

;; differential element accumulator keyed by ElementDefinition id
(define (ensure elems order id path slice)
  (if (hash-has-key? elems id)
      (values elems order)
      (values (hash-set elems id (drop-false (hasheq 'id id 'path path 'sliceName slice))) (cons id order))))
(define (upd elems order id path slice f)
  (define-values (e1 o1) (ensure elems order id path slice))
  (values (hash-set e1 id (f (hash-ref e1 id))) o1))

;; FSH assignment on a profile path -> fixed<Type> (primitive) or pattern<Type> (complex)
(define (apply-pattern h base-type rel-path v exact?)
  (define tc (or (and (type-registered? (string->symbol base-type))
                      (element-fhir-type (string->symbol base-type) rel-path))
                 (value-type-code v)))
  (cond
    [(not tc) h]
    [else
     (define complex? (char-upper-case? (string-ref tc 0)))
     (define ftype (if complex? (vector 'type (string->symbol tc)) (vector 'prim tc)))
     ;; FSH `=` assigns a PATTERN; `= … (exactly)` assigns a FIXED value (both for prim and complex)
     (define key (string->symbol
                  (string-append (if exact? "fixed" "pattern") (capitalize tc))))
     (hash-set h key (coerce v ftype))]))

(define (value-type-code v)
  (match v
    [(struct fsh-code _) "code"] [(struct fsh-quantity _) "Quantity"]
    [(struct fsh-reference _) "Reference"] [(? boolean? _) "boolean"]
    [(? number? _) "decimal"] [(? string? _) "string"] [_ #f]))

;; type->jsexpr : an `only` type -> ElementDefinition.type. Names resolve through Aliases and this
;; document's profiles: `only MyCodingProfile` = code Coding + profile [its url]; a Reference target
;; naming a local profile is that profile's canonical.
(define (type->jsexpr t)
  (match (regexp-match #px"^(Reference|Canonical|CodeableReference)\\((.*)\\)$" t)
    [(list _ "Canonical" inner) (hasheq 'code "canonical" 'targetProfile (target-urls inner))]
    [(list _ kind inner) (hasheq 'code kind 'targetProfile (target-urls inner))]
    [_ (define n (resolve-alias t (current-aliases)))
       (cond [(hash-ref (current-profiles) n #f)
              => (lambda (p) (hasheq 'code (instance-base n (current-profiles))
                                     'profile (list (local-profile-url p))))]
             [(regexp-match? #px"^https?:" n) (hasheq 'code (last-url-seg n) 'profile (list n))]
             [else (hasheq 'code n)])]))
(define (target-urls inner)
  (for/list ([s (in-list (regexp-split #px"\\s+or\\s+" (string-trim inner)))])
    (define n (resolve-alias (string-trim s) (current-aliases)))
    (cond [(hash-ref (current-profiles) n #f) => local-profile-url] [else (sd-url n)])))
(define (local-profile-url p) (string-append (canonical-base) "/StructureDefinition/" (cdr p)))
;; a ValueSet reference in a binding: alias, then this document's ValueSets, else as written
(define (valueset-url n)
  (define r (resolve-alias n (current-aliases)))
  (or ((current-local-canonical) r) (external-canonical "ValueSet" r) r))

;; external-canonical : kind name -> url | #f   a bare NAME (not a url) of a CodeSystem / ValueSet in
;; the core/dependency packages the SD index sees (`from system ConditionCategoryCodes`)
(define (external-canonical kind n)
  (and (string? n) (not (regexp-match? #rx"[:/]" n)) (sd-index-available?)
       (let ([r (resolve-canonical-raw kind n)]) (and r (hash-ref r 'url #f)))))

;; ---- Extension auto-scaffolding (SUSHI-style) ----------------------------------------------
;; a top-level `extension` (type-characteristics can-bind) + a `context` of Element
(define (add-extension-header sd)
  (hash-set* sd
             'context (list (hasheq 'type "element" 'expression "Element"))
             'extension (list (hasheq 'url "http://hl7.org/fhir/StructureDefinition/structuredefinition-type-characteristics"
                                      'valueCode "can-bind"))))

;; add the scaffold elements SUSHI generates for an Extension around the user's folded rules
(define (add-extension-scaffold elems order self-url meta)
  (define keys (hash-keys elems))
  (define slice-names (for/list ([k (in-list keys)]
                                 #:when (regexp-match? #px"^Extension\\.extension:[^.]+$" k))
                        (substring k (string-length "Extension.extension:"))))
  (define complex? (for/or ([k (in-list keys)]) (regexp-match? #px"^Extension\\.extension($|:)" k)))
  (define steps
    (append
     ;; root element carries the extension's short/definition
     (list (lambda (es os) (upd es os "Extension" "Extension" #f
                                (lambda (h) (hash-set* h 'short (assoc-ref meta 'title)
                                                       'definition (assoc-ref meta 'description))))))
     ;; simple extension: no sub-extensions allowed
     (if complex? '()
         (list (lambda (es os) (upd es os "Extension.extension" "Extension.extension" #f
                                    (lambda (h) (hash-set h 'max "0"))))))
     ;; per sub-extension slice: forbid nested extension, fix its url to the slice name
     (append-map
      (lambda (sn)
        (list (lambda (es os) (upd es os (format "Extension.extension:~a.extension" sn)
                                   "Extension.extension.extension" #f (lambda (h) (hash-set h 'max "0"))))
              (lambda (es os) (upd es os (format "Extension.extension:~a.url" sn)
                                   "Extension.extension.url" #f (lambda (h) (hash-set h 'fixedUri sn))))))
      slice-names)
     ;; the extension's own url is fixed to its canonical
     (list (lambda (es os) (upd es os "Extension.url" "Extension.url" #f
                               (lambda (h) (hash-set h 'fixedUri self-url)))))))
  (for/fold ([es elems] [os order]) ([step (in-list steps)]) (step es os)))

(define (apply-card h card)
  (match (regexp-match #px"^([0-9]*)\\.\\.([0-9]*|\\*)$" card)
    [(list _ lo hi) (drop-false (hash-set (hash-set h 'min (and (positive? (string-length lo)) (string->number lo)))
                                          'max (and (positive? (string-length hi)) hi)))]
    [_ h]))
(define mod-flag (string->symbol "?!"))
(define (apply-flags h flags)
  (for/fold ([h h]) ([f (in-list flags)])
    (cond [(eq? f 'MS) (hash-set h 'mustSupport #t)]
          [(eq? f 'SU) (hash-set h 'isSummary #t)]
          [(eq? f mod-flag) (hash-set h 'isModifier #t)]
          [else h])))

;; ---- Instance -> a plain resource ----------------------------------------------------------
(define (compile-instance e aliases profiles)
  (define meta (fsh-instance-metadata e))
  (define type-str (assoc-ref meta 'instanceOf))
  ;; resolve InstanceOf to a base resource type: a base type stays; a local profile -> its parent
  (define base (instance-base type-str profiles))
  (define t (string->symbol base))
  (define js (build-instance-json e t aliases))
  ;; stamp meta.profile when InstanceOf named a profile (not a base type), using the profile's id-URL
  (define prof (and (not (type-registered? (string->symbol type-str))) (hash-ref profiles type-str #f)))
  (if prof
      (inject-meta-profile js (string-append (canonical-base) "/StructureDefinition/" (cdr prof)))
      js))

;; follows the Parent chain (a profile of a profile of …) to the base resource type; bounded so a
;; cyclic Parent cannot loop
(define (instance-base type-str profiles [fuel 16])
  (cond [(type-registered? (string->symbol type-str)) type-str]
        [(and (> fuel 0) (hash-ref profiles type-str #f))
         => (lambda (p) (instance-base (let ([par (car p)]) (if (hash-ref profiles par #f) par (last-url-seg par)))
                                       profiles (sub1 fuel)))]
        [else type-str]))

(define (build-instance-json e resource-type aliases)
  (define base (hasheq 'resourceType (symbol->string resource-type) 'id (fsh-instance-name e)))
  (define assigns (for/list ([r (in-list (fsh-instance-rules e))] #:when (rule-assign? r)) r))
  (define of (assoc-ref (fsh-instance-metadata e) 'instanceOf))
  (define paths (resolve-soft-indices (for/list ([r (in-list assigns)]) (fsh-path-steps (parse-fsh-path (rule-assign-path r))))
                                      #:slice-order (if of ((current-slice-orders) of) (hash))))
  (for/fold ([js base]) ([r (in-list assigns)] [steps (in-list paths)])
    (assign js resource-type steps (resolve (rule-assign-value r) aliases))))

;; resolve-soft-indices : (listof (listof fsh-step)) -> same, with every `[+]`/`[=]` and every
;; named slice made a numeric index.
;; FSH semantics (https://hl7.org/fhir/uv/shorthand/reference.html#soft-indexing): an index counter
;; per array PATH PREFIX (the resolved path up to that element); `[+]` advances it (first use -> 0),
;; `[=]` reuses it, an explicit `[n]` sets it. In an Instance a SLICE NAME (`section[problems]`)
;; denotes one element of the array: its first mention takes the next index, later mentions reuse it.
;; Rules are processed in order.
(define (resolve-soft-indices paths #:slice-order [slice-order (hash)])
  (define counters (make-hash))
  (define slices (make-hash))                       ; (key . slice-name) -> index
  ;; a top-level array's named slices take positions in the PROFILE's declaration order (SUSHI), so
  ;; allocate them up front: the slices this instance uses, profile order first, then any others
  (define used (make-hash))                         ; key -> slice names, first mention order
  (for ([steps (in-list paths)] #:when (pair? steps))
    (define st (car steps))
    (for ([b (in-list (fsh-step-brackets st))] #:when (eq? (fsh-bracket-kind b) 'slice))
      (define key (string-append "." (fsh-step-name st)))
      (unless (member (fsh-bracket-value b) (hash-ref used key '()))
        (hash-update! used key (lambda (l) (append l (list (fsh-bracket-value b)))) '()))))
  (for ([(key names) (in-hash used)])
    (define declared (hash-ref slice-order (substring key 1) '()))
    (define ordered (append (filter (lambda (n) (member n names)) declared)
                            (filter (lambda (n) (not (member n declared))) names)))
    (when (pair? declared)
      (for ([n (in-list ordered)] [i (in-naturals)]) (hash-set! slices (cons key n) i))
      (hash-set! counters key (sub1 (length ordered)))))
  (for/list ([steps (in-list paths)])
    (let loop ([steps steps] [prefix ""] [acc '()])
      (cond
        [(null? steps) (reverse acc)]
        [else
         (define st (car steps))
         (define key (string-append prefix "." (fsh-step-name st)))
         (define brackets
           (for/list ([b (in-list (fsh-step-brackets st))])
             (case (fsh-bracket-kind b)
               [(soft+) (let ([n (if (hash-has-key? counters key) (add1 (hash-ref counters key)) 0)])
                          (hash-set! counters key n) (fsh-bracket 'index n))]
               [(soft=) (fsh-bracket 'index (hash-ref counters key 0))]
               [(index) (hash-set! counters key (fsh-bracket-value b)) b]
               [(slice)
                (define sk (cons key (fsh-bracket-value b)))
                (cond [(hash-ref slices sk #f) => (lambda (n) (hash-set! counters key n) (fsh-bracket 'index n))]
                      [else (define n (if (hash-has-key? counters key) (add1 (hash-ref counters key)) 0))
                            (hash-set! counters key n) (hash-set! slices sk n) (fsh-bracket 'index n)])]
               [else b])))
         (define resolved (fsh-step (fsh-step-name st) brackets))
         (define next-prefix
           (string-append key (apply string-append
                                     (for/list ([b (in-list brackets)])
                                       (format "[~a]" (or (fsh-bracket-value b) (fsh-bracket-kind b)))))))
         (loop (cdr steps) next-prefix (cons resolved acc))]))))

(define (inject-meta-profile js url)
  (if (and url (hash? js))
      (hash-update js 'meta (lambda (m) (hash-set (if (hash? m) m (hasheq)) 'profile (list url))) (hasheq))
      js))

;; assign `value` at `steps` into jsexpr `js` of type `type-name`
(define (assign js type-name steps value)
  (match steps
    ['() value]
    [(list step)
     (define-values (kind ftype) (field-info type-name (step-sym step)))
     (set-field js (step-sym step) kind (step-index step) (coerce value ftype))]
    [(cons step more)
     (define-values (kind ftype) (field-info type-name (step-sym step)))
     (cond
       ;; `display.extension…` / `given[0].id`: extensions and ids of a PRIMITIVE live in its `_name`
       ;; sibling (https://hl7.org/fhir/json.html#primitive) — an object for a single primitive, an
       ;; array aligned with the value array (null placeholders) for a repeating one
       [(and (eq? (ftype-tag ftype) 'prim) (memq (step-sym (car more)) '(extension id)))
        (define sib (string->symbol (string-append "_" (fsh-step-name step))))
        (define (into child) (assign (if (hash? child) child (hasheq)) 'Extension more value))
        (case kind
          [(list)
           (define n (length (hash-ref js (step-sym step) '())))
           (define idx (let ([i (step-index step)]) (if (exact-nonnegative-integer? i) i (max 0 (sub1 n)))))
           (define cur (hash-ref js sib '()))
           (define padded (append cur (make-list (max 0 (- (add1 idx) (length cur))) 'null)))
           (hash-set js sib (for/list ([x (in-list padded)] [j (in-naturals)]) (if (= j idx) (into x) x)))]
          [else (hash-set js sib (into (hash-ref js sib #f)))])]
       [else
        (update-field js (step-sym step) kind (step-index step)
                      (lambda (child) (assign (or child (hasheq)) (ftype->type ftype) more value)))])]))

(define (step-sym s) (string->symbol (fsh-step-name s)))
(define (step-index s)
  (for/or ([b (in-list (fsh-step-brackets s))])
    (case (fsh-bracket-kind b) [(index) (fsh-bracket-value b)] [(soft+ soft=) 'end] [else #f])))

(define (set-field js fname kind idx v)
  (case kind
    [(list) (hash-set js fname (list-set* (hash-ref js fname '()) idx v))]
    [else   (hash-set js fname v)]))
(define (update-field js fname kind idx f)
  (case kind
    [(list)
     (define cur (hash-ref js fname '()))
     (define i (if (eq? idx 'end) (max 0 (sub1 (length cur))) (or idx (if (null? cur) 0 (sub1 (length cur))))))
     (hash-set js fname (list-update* cur i f))]
    [else (hash-set js fname (f (hash-ref js fname #f)))]))
(define (list-set* lst idx v)
  (define i (if (or (eq? idx 'end) (not idx)) (length lst) idx))
  (list-pad lst i v))
(define (list-update* lst i f)
  (let ([lst (extend lst (add1 i))]) (for/list ([x (in-list lst)] [j (in-naturals)]) (if (= j i) (f x) x))))
(define (list-pad lst i v)
  (let ([lst (extend lst (add1 i))]) (for/list ([x (in-list lst)] [j (in-naturals)]) (if (= j i) v x))))
(define (extend lst n) (append lst (make-list (max 0 (- n (length lst))) #f)))

(define (ftype->type ftype)
  (case (ftype-tag ftype) [(type) (ftype-type-name ftype)] [else 'object]))

;; coerce an FSH value to jsexpr given the target ftype
(define (coerce v ftype)
  (define tag (ftype-tag ftype))
  (match* (tag v)
    [('prim (struct fsh-code (c _))) (strip# c)]
    [(_ (struct fsh-code (c d)))
     (define-values (sys code) (split-code c))
     (case (and (eq? tag 'type) (ftype-type-name ftype))
       [(Coding)          (drop-false (hasheq 'system sys 'code code 'display d))]
       [(CodeableConcept) (hasheq 'coding (list (drop-false (hasheq 'system sys 'code code 'display d))))]
       [else (or code (strip# c))])]
    [(_ (struct fsh-quantity (val u d)))
     (drop-false (hasheq 'value val 'unit (or d u) 'code u 'system "http://unitsofmeasure.org"))]
    [(_ (struct fsh-reference (t d))) (drop-false (hasheq 'reference t 'display d))]
    [(_ (struct fsh-canonical (t))) t]
    ;; a bare instance name assigned to a Resource-typed element (`* entry[0].resource = MyPatient`,
    ;; `* contained[0] = Med1`) embeds that instance (https://hl7.org/fhir/uv/shorthand/reference.html#assignments-with-resources)
    [('resource (? string? s)) (or ((current-instance-resolver) s) s)]
    [(_ (? boolean? b)) b] [(_ (? number? n)) n] [(_ (? string? s)) s] [(_ _) v]))

;; current-instance-resolver : (parameter (string -> (or jsexpr #f)))   name/id -> the compiled instance
(define current-instance-resolver (make-parameter (lambda (_) #f)))

;; ---- ValueSet / CodeSystem -----------------------------------------------------------------
(define (compile-valueset e aliases)
  (define meta (fsh-valueset-metadata e))
  (define id (or (assoc-ref meta 'id) (fsh-valueset-name e)))
  (define-values (incl excl)
    (for/fold ([incl '()] [excl '()]) ([r (in-list (fsh-valueset-rules e))])
      (match r
        [(vs-component include? body)
         (define comp (vs-body->jsexpr body aliases))
         (if include? (values (cons comp incl) excl) (values incl (cons comp excl)))]
        [_ (values incl excl)])))
  (apply-carets
   (drop-false
    (hasheq 'resourceType "ValueSet" 'id id
            'url (string-append (canonical-base) "/ValueSet/" id)
            'version "0.1.0" 'name (fsh-valueset-name e) 'status "active"
            'title (assoc-ref meta 'title) 'description (assoc-ref meta 'description)
            'compose (drop-false (hasheq 'include (and (pair? incl) (merge-concept-components (reverse incl)))
                                         'exclude (and (pair? excl) (merge-concept-components (reverse excl)))))))
   (fsh-valueset-rules e)))

;; merge-concept-components : (listof component) -> (listof component)
;; SUSHI adds an enumerated concept to the existing component for the same system+version (one
;; that enumerates concepts and has no filter / valueSet); components keep first-seen order.
(define (merge-concept-components comps)
  (define (key c) (and (hash-has-key? c 'concept) (not (hash-has-key? c 'filter)) (not (hash-has-key? c 'valueSet))
                       (list (hash-ref c 'system #f) (hash-ref c 'version #f))))
  (for/fold ([out '()] #:result (reverse out)) ([c (in-list comps)])
    (define k (key c))
    (define existing (and k (findf (lambda (o) (equal? (key o) k)) out)))
    (if existing
        (map (lambda (o) (if (eq? o existing) (hash-set o 'concept (append (hash-ref o 'concept) (hash-ref c 'concept))) o)) out)
        (cons c out))))

;; vs-body->jsexpr : vsComponent body datum -> ValueSet.compose.include|exclude component
;;   `from system X[|ver]`  -> system (+ version);  `from valueset A and B` -> valueSet [A B]
;;   `where p op v and …`    -> filter [{property op value}];  enumerated codes -> concept [{code display}]
;; Names resolve through Aliases, then through this document's own ValueSet/CodeSystem entities
;; (name or id -> canonical URL), as SUSHI does.
(define (vs-body->jsexpr body aliases)
  (define tree (cons 'root body))
  (define (canon n [kind "CodeSystem"])
    (let ([r (resolve-alias n aliases)]) (or ((current-local-canonical) r) (external-canonical kind r) r)))
  (define (split-version u) (match (regexp-match #px"^([^|]*)[|](.*)$" u) [(list _ a v) (values a v)] [#f (values u #f)]))
  (define from-system
    (for/or ([n (in-list (find-nodes tree 'vsFromSystem))])
      (for/or ([x (in-list (cdr n))]) (and (pair? x) (eq? (car x) 'name) (canon (cadr x))))))
  (define value-sets
    (for*/list ([n (in-list (find-nodes tree 'vsFromValueset))]
                [x (in-list (cdr n))] #:when (and (pair? x) (eq? (car x) 'name)))
      (canon (cadr x) "ValueSet")))
  (define filters
    (for/list ([d (in-list (find-nodes tree 'vsFilterDefinition))])
      (match-define (list _ (list 'name prop) (list 'vsFilterOperator op) val ...) d)
      (drop-false (hasheq 'property prop 'op op 'value (and (pair? val) (filter-value (car val)))))))
  (define codes (find-nodes (cons 'root (find-nodes tree 'vsConceptComponent)) 'code))
  (define split (for/list ([c (in-list codes)]) (call-with-values (lambda () (split-code (code-text* (cadr c)))) cons)))
  (define sys+ver (or from-system (and (pair? split) (car (car split)) (canon (car (car split))))))
  (define-values (sys ver) (if sys+ver (split-version sys+ver) (values #f #f)))
  (define concepts
    (for/list ([c (in-list codes)] [sp (in-list split)])
      (drop-false (hasheq 'code (cdr sp) 'display (code-display c)))))
  (drop-false (hasheq 'system sys 'version ver
                      'concept (and (pair? concepts) concepts)
                      'filter (and (pair? filters) filters)
                      'valueSet (and (pair? value-sets) value-sets))))

;; a vsFilterValue datum -> the filter's value string
(define (filter-value v)
  (match v
    [(list 'vsFilterValue (list 'code c _ ...)) (let-values ([(_s code) (split-code (code-text* c))]) code)]
    [(list 'vsFilterValue (? string? s))
     (cond [(regexp-match #px"^/(.*)/$" s) => cadr] [else s])]
    [(list 'vsFilterValue x) (format "~a" x)]
    [_ #f]))

;; current-local-canonical : (parameter (string -> (or url #f)))  this document's ValueSet/CodeSystem
;; names and ids -> canonical URLs
(define current-local-canonical (make-parameter (lambda (_) #f)))

;; a (code CODE STRING?) datum's optional display string
(define (code-display c) (for/or ([x (in-list (cddr c))] #:when (string? x)) x))

(define (find-nodes tree tag)
  (cond [(and (pair? tree) (eq? (car tree) tag)) (list tree)]
        [(pair? tree) (append (find-nodes (car tree) tag) (find-nodes (cdr tree) tag))]
        [else '()]))

(define (compile-codesystem e aliases)
  (define meta (fsh-codesystem-metadata e))
  (define id (or (assoc-ref meta 'id) (fsh-codesystem-name e)))
  ;; `* #a #b "..."` nests concept b under a; concepts accumulate into a hierarchy tree
  (define concepts
    (for/fold ([tree '()]) ([r (in-list (fsh-codesystem-rules e))] #:when (concept-rule? r))
      (insert-concept tree (map strip# (concept-rule-codes r))
                      (concept-rule-display r) (concept-rule-definition r))))
  (apply-carets
   (drop-false
    (hasheq 'resourceType "CodeSystem" 'id id
            'url (string-append (canonical-base) "/CodeSystem/" id)
            'version "0.1.0" 'name (fsh-codesystem-name e) 'status "active"
            'title (assoc-ref meta 'title) 'description (assoc-ref meta 'description)
            'content "complete" 'concept (and (pair? concepts) concepts)
            'count (and (pair? concepts) (count-concepts concepts))))
   (fsh-codesystem-rules e)))

;; insert a code (chain of ancestor codes ending in the defined code) into the concept tree
(define (insert-concept concepts codes display def)
  (define code (car codes))
  (define rest (cdr codes))
  (define existing (findf (lambda (c) (equal? (hash-ref c 'code) code)) concepts))
  (define base (or existing (hasheq 'code code)))
  (define updated
    (if (null? rest)
        (drop-false (hash-set (hash-set base 'display display) 'definition def))
        (hash-set base 'concept (insert-concept (hash-ref base 'concept '()) rest display def))))
  (if existing
      (for/list ([c (in-list concepts)]) (if (equal? (hash-ref c 'code) code) updated c))
      (append concepts (list updated))))

(define (count-concepts concepts)
  (for/sum ([c (in-list concepts)]) (+ 1 (count-concepts (hash-ref c 'concept '())))))

;; ---- Mapping -> StructureDefinition.mapping + ElementDefinition.mapping (into the Source SD) ----
(define (apply-mapping sds m)
  (define meta (fsh-mapping-metadata m))
  (define src (assoc-ref meta 'source))
  (define mid (or (assoc-ref meta 'id) (fsh-mapping-name m)))
  (define decl (drop-false (hasheq 'identity mid 'name (assoc-ref meta 'title) 'uri (assoc-ref meta 'target))))
  (for/list ([sd (in-list sds)])
    (if (and (equal? (hash-ref sd 'resourceType #f) "StructureDefinition")
             (or (equal? (hash-ref sd 'name #f) src) (equal? (hash-ref sd 'id #f) src)))
        (inject-mapping sd mid decl (fsh-mapping-rules m))
        sd)))

(define (inject-mapping sd mid decl rules)
  (define base-type (hash-ref sd 'type))
  (define sd1 (hash-update sd 'mapping (lambda (ms) (append (if (list? ms) ms '()) (list decl))) '()))
  (define diff (hash-ref (hash-ref sd1 'differential (hasheq)) 'element '()))
  (define diff2
    (for/fold ([elems diff]) ([r (in-list rules)] #:when (and (rule-mapping? r) (rule-mapping-path r)))
      (define-values (eid epath _s) (element-loc base-type (rule-mapping-path r)))
      (define entry (drop-false (hasheq 'identity mid 'map (rule-mapping-target r)
                                        'comment (rule-mapping-comment r)
                                        'language (and (rule-mapping-code r) (strip# (rule-mapping-code r))))))
      (add-element-mapping elems eid epath entry)))
  (hash-set sd1 'differential (hasheq 'element diff2)))

;; append a mapping entry to the element with `eid` (adding a bare element if it isn't in the diff yet)
(define (add-element-mapping elems eid epath entry)
  (cond
    [(findf (lambda (e) (equal? (hash-ref e 'id #f) eid)) elems)
     (for/list ([e (in-list elems)])
       (if (equal? (hash-ref e 'id #f) eid)
           (hash-update e 'mapping (lambda (ms) (append (if (list? ms) ms '()) (list entry))) '())
           e))]
    [else (append elems (list (hasheq 'id eid 'path epath 'mapping (list entry))))]))

;; ---- Invariant -----------------------------------------------------------------------------
(define (compile-invariant e)
  (define meta (fsh-invariant-metadata e))
  (define expr (assoc-ref meta 'expression))
  (hash-set
   (drop-false
    (hasheq 'resourceType "Invariant" 'key (fsh-invariant-name e)
            'severity (strip# (or (assoc-ref meta 'severity) "#error"))
            'human (assoc-ref meta 'description)
            'expression expr))
   'expressionValid
   (and expr (with-handlers ([(lambda (_) #t) (lambda (_) #f)]) (parse-fhirpath expr) #t))))

;; ---- caret application (SD/VS/CS root-level, path #f) ---------------------------------------
;; root-level carets (`* ^status = #active`), set by the RESOURCE's schema (caret.rkt typed mode):
;; repeating elements, soft indices, extension[url], and values shaped for their target type
(define (apply-carets js rules)
  (define type (and (hash? js) (let ([t (hash-ref js 'resourceType #f)]) (and t (string->symbol t)))))
  (define counters (make-hash))
  (for/fold ([js js]) ([r (in-list rules)])
    (match r
      [(rule-caret #f cp v) (typed-caret-set js cp v type counters)]
      [(rule-code-caret '() cp v) (typed-caret-set js cp v type counters)]
      [_ js])))

;; typed-caret-set : jsexpr caret-path fsh-value type counters -> jsexpr
(define (typed-caret-set js cp v type counters)
  (if (and type (type-registered? type))
      (caret-set js cp v #:type type #:counters counters
                 #:coerce (lambda (val ftype) (coerce (resolve val (current-aliases)) ftype)))
      (caret-set js cp (caret-val v))))

;; current-aliases : the document's Alias table (set by compile-doc/errors) — carets resolve through it
(define current-aliases (make-parameter (hash)))
;; current-slice-orders : profile name/id -> (hash element-path -> slice names in declaration order)
(define current-slice-orders (make-parameter (lambda (_) (hash))))
;; current-profiles : the document's Profiles/Extensions by name/id/url -> (cons parent id)
(define current-profiles (make-parameter (hash)))

;; inherited-mappings : base type -> the core SD's `mapping` list, or #f when no core package is
;; available to the SD index (fail open: the profile then simply carries none)
(define (inherited-mappings base-type)
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define m (and (hash? sd) (hash-ref sd 'mapping #f)))
  (and (list? m) (pair? m) m))
(define (caret-val v)
  (match v
    [(struct fsh-code (c _)) (strip# c)]
    [(? string? s) s] [(? boolean? b) b] [(? number? n) n]
    [_ (format "~a" v)]))

;; ---- misc ----------------------------------------------------------------------------------
(define (assoc-ref alist key) (cond [(assq key alist) => cdr] [else #f]))
(define (resolve v aliases)
  (match v
    [(struct fsh-reference (t d)) (fsh-reference (resolve-reference (resolve-alias t aliases) aliases) d)]
    ;; `$sct#123` / `SCT#123`: the system part of a code is an alias, as in rules and value sets
    [(struct fsh-code (c d))
     (match (regexp-match #px"^([^#]+)#(.*)$" c)
       [(list _ sys code) (fsh-code (string-append (resolve-alias sys aliases) "#" code) d)]
       [#f v])]
    [(? string? s) (resolve-alias s aliases)]
    [_ v]))
(define (resolve-alias s aliases) (if (and (string? s) (hash-has-key? aliases s)) (hash-ref aliases s) s))

;; resolve-reference : "Reference(X)" | "X" -> the reference string. X naming an instance of this
;; document becomes "<ResourceType>/<id>" (SUSHI); anything else (Type/id, a URL, an alias) stays.
(define (resolve-reference t aliases)
  (define inner (match (regexp-match #px"^\\s*Reference\\s*\\(\\s*(.*?)\\s*\\)\\s*$" t)
                  [(list _ x) (resolve-alias x aliases)] [#f t]))
  (or ((current-instance-ref) inner) inner))

;; current-instance-ref : (parameter (string -> (or "Type/id" #f)))
(define current-instance-ref (make-parameter (lambda (_) #f)))
(define (code-text* x) (if (code-tok? x) (code-tok-text x) x))
(define (strip# s) (if (and (string? s) (regexp-match? #px"^#" s)) (substring s 1) s))
(define (split-code c)
  (define m (regexp-match #px"^([^#]*)#(.*)$" c))
  (if m (values (let ([s (cadr m)]) (and (positive? (string-length s)) s)) (caddr m)) (values #f c)))
(define (capitalize s)
  (if (= 0 (string-length s)) s (string-append (string-upcase (substring s 0 1)) (substring s 1))))
(define (sd-url t)
  (if (regexp-match? #px"^https?:" t) t (string-append "http://hl7.org/fhir/StructureDefinition/" t)))
(define (last-url-seg s) (if (regexp-match? #rx"/" s) (last (string-split s "/")) s))
