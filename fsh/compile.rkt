#lang racket/base

;;; compile.rkt — FSH entity ASTs -> FHIR JSON (jsexpr).
;;;
;;; Adapted from the stroma FSH compiler to this repo's information model. Profiles/Extensions compile
;;; to a StructureDefinition DIFFERENTIAL (no snapshot — deferred); Instances to a plain resource
;;; (schema-driven array-vs-object nesting via resolve.rkt); ValueSet/CodeSystem to their compose /
;;; concept JSON; Invariant Expressions are checked with the FHIRPath parser. Targets R5
;;; (fhirVersion "5.0.0", canonical base default http://example.org) to match the SUSHI golden corpus.
;;;
;;; The jsexpr is the faithful output (compared against SUSHI); fsh/base.rkt additionally realises each
;;; as a typed node via from-json.

(require racket/match
         racket/string
         racket/contract
         (only-in racket/list append-map make-list last filter-map)
         "parser/ast.rkt"
         "parser/path.rkt"
         "caret.rkt"
         (only-in "parser/parse.rkt" parse-fsh-doc)
         (only-in "resolve.rkt" element-fhir-type field-info)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/model/schema ftype-tag ftype-type-name)
         (only-in fhir/serialize/jsexpr-util drop-false)
         (only-in fhir/fhirpath/parser/parse parse-fhirpath)
         (only-in fhir/sd/snapshot generate-snapshot))

(provide (contract-out
          [canonical-base (parameter/c string?)]
          [emit-snapshot? (parameter/c boolean?)]
          [compile-entity (->* (struct?) (hash? hash? hash?) (or/c hash? #f))]
          [compile-doc    (-> (listof struct?) (listof hash?))]
          [fsh->fhir      (-> string? (listof hash?))]))

;; the IG canonical URL base (SUSHI default in the golden corpus is http://example.org)
(define canonical-base (make-parameter "http://example.org"))
(define fhir-version "5.0.0")
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
(define (compile-doc entities)
  (define aliases  (for/hash ([e (in-list entities)] #:when (fsh-alias? e))
                     (values (fsh-alias-name e) (fsh-alias-value e))))
  (define rulesets (for/hash ([e (in-list entities)] #:when (fsh-ruleset? e))
                     (values (fsh-ruleset-name e) (fsh-ruleset-rules e))))
  ;; parameterized RuleSets: name -> (cons params body-tokens)
  (define param-rulesets (for/hash ([e (in-list entities)] #:when (fsh-param-ruleset? e))
                           (values (fsh-param-ruleset-name e)
                                   (cons (fsh-param-ruleset-params e) (fsh-param-ruleset-body e)))))
  ;; profile name -> (cons parent-base profile-id) — for Instance base resolution + meta.profile
  (define profiles (for/hash ([e (in-list entities)]
                              #:when (or (fsh-profile? e) (fsh-extension? e)))
                     (values (entity-name e)
                             (cons (or (assoc-ref (entity-meta e) 'parent)
                                       (if (fsh-extension? e) "Extension" "DomainResource"))
                                   (or (assoc-ref (entity-meta e) 'id) (entity-name e))))))
  (define invariants (for/hash ([e (in-list entities)] #:when (fsh-invariant? e))
                       (define m (fsh-invariant-metadata e))
                       (values (fsh-invariant-name e)
                               (hasheq 'severity (strip# (or (assoc-ref m 'severity) "#error"))
                                       'human (assoc-ref m 'description)
                                       'expression (assoc-ref m 'expression)))))
  ;; Invariants are consumed only as SD constraint sources (like SUSHI) — not emitted standalone.
  (define compiled
    (for*/list ([e (in-list entities)]
                #:unless (or (fsh-alias? e) (fsh-ruleset? e) (fsh-param-ruleset? e)
                             (fsh-invariant? e) (fsh-mapping? e))
                [out (in-value (compile-entity (expand-entity-inserts e rulesets param-rulesets) aliases profiles invariants))]
                #:when out)
      out))
  ;; Mapping entities inject into their Source StructureDefinition (SUSHI-style) — a post-pass.
  (for/fold ([sds compiled]) ([m (in-list entities)] #:when (fsh-mapping? m))
    (apply-mapping sds m)))

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
  (define parent (or (assoc-ref meta 'parent) (if ext? "Extension" "DomainResource")))
  (define base-type (if ext? "Extension" (last-url-seg parent)))
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
                        'fhirVersion fhir-version
                        'kind (if ext? "complex-type" "resource")
                        'type base-type
                        'baseDefinition (sd-url parent)
                        'derivation "constraint"
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
              'status "active" 'fhirVersion fhir-version
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
  (for/fold ([elems (hash)] [order '()]) ([r (in-list rules)])
    (define p (rule-path-of r))
    (define-values (rid rpath rslice)
      (if (string? p) (element-loc base-type p) (values #f #f #f)))
    (match r
      [(rule-card p card flags) (upd elems order rid rpath rslice (lambda (h) (apply-card (apply-flags h flags) card)))]
      [(rule-flag p _ flags)    (upd elems order rid rpath rslice (lambda (h) (apply-flags h flags)))]
      [(rule-binding p vs str)  (upd elems order rid rpath rslice (lambda (h) (hash-set h 'binding (drop-false (hasheq 'strength (and str (symbol->string str)) 'valueSet vs)))))]
      [(rule-only p types)      (upd elems order rid rpath rslice (lambda (h) (hash-set h 'type (map type->jsexpr types))))]
      [(rule-assign p v exact?) (upd elems order rid rpath rslice (lambda (h) (apply-pattern h base-type p v exact?)))]
      [(rule-caret p cp v)      (if p (upd elems order rid rpath rslice (lambda (h) (caret-set h cp (caret-val v)))) (values elems order))]
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
  (define steps (fsh-path-steps (parse-fsh-path fsh-path-str)))
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

(define (type->jsexpr t)
  (match (regexp-match #px"^(Reference|Canonical|CodeableReference)\\((.*)\\)$" t)
    [(list _ "Canonical" inner) (hasheq 'code "canonical" 'targetProfile (target-urls inner))]
    [(list _ kind inner) (hasheq 'code kind 'targetProfile (target-urls inner))]
    [_ (hasheq 'code t)]))
(define (target-urls inner)
  (for/list ([s (in-list (regexp-split #px"\\s+or\\s+" (string-trim inner)))]) (sd-url (string-trim s))))

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

(define (instance-base type-str profiles)
  (cond [(type-registered? (string->symbol type-str)) type-str]
        [(hash-ref profiles type-str #f) => (lambda (p) (last-url-seg (car p)))]
        [else type-str]))

(define (build-instance-json e resource-type aliases)
  (define base (hasheq 'resourceType (symbol->string resource-type) 'id (fsh-instance-name e)))
  (for/fold ([js base]) ([r (in-list (fsh-instance-rules e))])
    (match r
      [(rule-assign p v _exact)
       (assign js resource-type (fsh-path-steps (parse-fsh-path p)) (resolve v aliases))]
      [_ js])))

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
     (update-field js (step-sym step) kind (step-index step)
                   (lambda (child) (assign (or child (hasheq)) (ftype->type ftype) more value)))]))

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
    [(_ (? boolean? b)) b] [(_ (? number? n)) n] [(_ (? string? s)) s] [(_ _) v]))

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
            'compose (drop-false (hasheq 'include (and (pair? incl) (reverse incl))
                                         'exclude (and (pair? excl) (reverse excl))))))
   (fsh-valueset-rules e)))

(define (vs-body->jsexpr body aliases)
  (define names (find-nodes (cons 'root body) 'name))
  (define codes (find-nodes (cons 'root body) 'code))
  ;; the system comes from a `from system X` clause, else from the code token's own system prefix
  (define split (for/list ([c (in-list codes)]) (call-with-values (lambda () (split-code (code-text* (cadr c)))) cons)))
  (define sys (or (and (pair? names) (resolve-alias (cadr (car names)) aliases))
                  (and (pair? split) (resolve-alias (car (car split)) aliases))))
  (define concepts
    (for/list ([c (in-list codes)] [s (in-list split)])
      (drop-false (hasheq 'code (cdr s) 'display (code-display c)))))
  (drop-false (hasheq 'system sys 'concept (and (pair? concepts) concepts))))

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
(define (apply-carets js rules)
  (for/fold ([js js]) ([r (in-list rules)])
    (match r
      [(rule-caret #f cp v) (caret-set js cp (caret-val v))]
      [(rule-code-caret '() cp v) (caret-set js cp (caret-val v))]
      [_ js])))
(define (caret-val v)
  (match v
    [(struct fsh-code (c _)) (strip# c)]
    [(? string? s) s] [(? boolean? b) b] [(? number? n) n]
    [_ (format "~a" v)]))

;; ---- misc ----------------------------------------------------------------------------------
(define (assoc-ref alist key) (cond [(assq key alist) => cdr] [else #f]))
(define (resolve v aliases)
  (match v
    [(struct fsh-reference (t d)) (fsh-reference (resolve-alias t aliases) d)]
    [(? string? s) (resolve-alias s aliases)]
    [_ v]))
(define (resolve-alias s aliases) (if (and (string? s) (hash-has-key? aliases s)) (hash-ref aliases s) s))
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
