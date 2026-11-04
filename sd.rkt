#lang racket/base
;;; sd.rkt — Profile / Extension -> StructureDefinition.differential, Logical / Resource ->
;;; StructureDefinition (specialization), extension auto-scaffolding (SUSHI-style), Mapping and
;;; Invariant; `element-loc` maps FSH paths to ElementDefinition ids.
(require racket/match
         racket/string
         racket/contract
         (only-in racket/list append-map make-list last filter-map remove-duplicates index-of drop-right)
         "parser/ast.rkt"
         "parser/path.rkt"
         "caret.rkt"
         (only-in "parser/parse.rkt" parse-fsh-doc)
         (only-in fhir/diagnostic diagnostic attached-loc)
         (only-in "resolve.rkt" element-fhir-type field-info)
         (only-in fhir/model/registry type-schema-of)
         (only-in fhir/model/schema type-schema-fields type-schema-parent schema-field-name schema-field-choice-base)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/model/schema ftype-tag ftype-type-name ftype-prim-code)
         (only-in fhir/serialize/jsexpr-util drop-false)
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhir/sd/snapshot generate-snapshot)
         (only-in fhir/sd/sd-index resolve-sd-raw resolve-canonical-raw sd-index-available?)
         (only-in fhir/sd/logical register-structure-definition!)
         "context.rkt"
         "instance.rkt"
         "caret-apply.rkt")
(provide apply-mapping card-lo compile-invariant compile-logical-resource compile-profile rule-path-of)
;; attach a snapshot to a built StructureDefinition when emit-snapshot? is on
(define (maybe-snapshot sd)
  (if (emit-snapshot?)
      (let ([snap (generate-snapshot sd)]) (if (pair? snap) (hash-set sd 'snapshot (hasheq 'element snap)) sd))
      sd))
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
  (define parent-url (cond [local-parent (string-append (canonical-base) "/StructureDefinition/" (cdr local-parent))]
                           [(external-sd parent) => (lambda (sd) (hash-ref sd 'url))]
                           [else (sd-url parent)]))
  (define id (or (assoc-ref meta 'id) name))
  (define self-url (string-append (canonical-base) "/StructureDefinition/" id))
  (define-values (elems0 order0) (fold-sd-rules base-type rules invariants self-url))
  (define-values (elems order)
    (if ext? (add-extension-scaffold elems0 order0 self-url meta) (values elems0 order0)))
  (define diff (snapshot-order base-type (for/list ([k (in-list (reverse order))]) (hash-ref elems k))))
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
                        ;; a profile shares its base's kind: Coding/CodeableConcept profiles are complex-type
                        'kind (cond [ext? "complex-type"]
                                    [(let ([b (and (sd-index-available?) (resolve-sd-raw base-type))])
                                       (and (hash? b) (hash-ref b 'kind #f)))]
                                    [(and (type-registered? (string->symbol base-type))
                                          (not (char-upper-case? (string-ref base-type 0)))) "primitive-type"]
                                    [else "resource"])
                        'type base-type
                        'baseDefinition parent-url
                        'derivation "constraint"
                        ;; SUSHI carries the base definition's mapping declarations into the profile
                        'mapping (inherited-mappings base-type)
                        'differential (and (pair? diff) (hasheq 'element diff)))))
  (define own-context? (for/or ([r (in-list rules)])
                         (and (rule-caret? r) (member (rule-caret-path r) '(#f "" "."))
                              (regexp-match? #rx"^\\^?context" (rule-caret-caret-path r)))))
  (define base* (let ([b (if ext? (add-extension-header base (not own-context?)) base)])
                  (if (hash-ref b 'context #f) b (hash-remove b 'context))))
  (maybe-snapshot (apply-carets (hash-set base* 'abstract #f) rules)))
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
                        'xpath (and inv (hash-ref inv 'xpath #f))
                        'source self-url)))
  (define element-counters (make-hash))      ; element id -> soft-index counters for its carets
  (for/fold ([elems (hash)] [order '()]) ([r (in-list rules)])
    (define p (rule-path-of r))
    (define-values (rid rpath rslice)
      (let-values ([(id path slice) (if (string? p) (element-loc base-type p) (values #f #f #f))])
        (if id (narrowed-choice elems id path slice) (values id path slice))))
    (define-values (elems* order*)
      (if (and rid (regexp-match? #px"\\[x\\]:" rid) (not (rule-only? r)))
          (let-values ([(e o) (ensure-choice-parents elems order rid)])
            ;; a type slice SUSHI creates restricts the choice to that one type: min 0, the base max
            (if (hash-has-key? e rid)
                (values e o)
                (upd e o rid rpath rslice
                     (lambda (h) (choice-slice-defaults h base-type rpath rslice)))))
          (values elems order)))
    (let ([elems elems*] [order order*])
    (match r
      [(rule-card p card flags) (upd elems order rid rpath rslice (lambda (h) (apply-card (apply-flags h flags) card)))]
      [(rule-flag p _ flags)    (upd elems order rid rpath rslice (lambda (h) (apply-flags h flags)))]
      [(rule-binding p vs str)  (upd elems order rid rpath rslice (lambda (h) (hash-set h 'binding (drop-false (hasheq 'strength (and str (symbol->string str)) 'valueSet (and vs (valueset-url vs)))))))]
      [(rule-only p types)
       ;; types in the base element's order (SUSHI), unknown ones after
       (define base-codes (or (base-type-codes base-type rpath) '()))
       (define (rank t) (or (index-of base-codes (hash-ref t 'code)) 1000))
       (define new-types (sort (map type->jsexpr types) < #:key rank))
       ;; an `only` naming exactly the base element's types narrows nothing: SUSHI emits no element
       (if (and (not (hash-has-key? elems rid)) (equal? (base-type-codes base-type rpath) (map (lambda (t) (hash-ref t 'code)) new-types))
                (andmap (lambda (t) (= (hash-count t) 1)) new-types))
           (values elems order)
           (upd elems order rid rpath rslice (lambda (h) (hash-set h 'type new-types))))]
      [(rule-assign p v exact?) (upd elems order rid rpath rslice (lambda (h) (apply-pattern h base-type p (resolve v (current-aliases)) exact?)))]
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
       (define ext? (regexp-match? #rx"[.](modifierE|e)xtension$" rpath))
       (define-values (e1 o1) (upd elems order rid rpath rslice
                                   (lambda (h)
                                     (let* ([h (if (> slice-min 0) (hash-set h 'min slice-min) h)]
                                            ;; SUSHI slices extensions by url when `contains` names them
                                            [h (if (and ext? (not (hash-has-key? h 'slicing))) (hash-set h 'slicing url-slicing) h)])
                                       h))))
       (for/fold ([elems e1] [order o1]) ([it (in-list items)])
         (define sn (or (contains-item-named-as it) (contains-item-name it)))
         ;; reslicing: when the contains target is itself a slice, sub-slice ids join with "/"
         (define sub-id (if rslice (string-append rid "/" sn) (string-append rid ":" sn)))
         (define sub-name (if rslice (string-append rslice "/" sn) sn))
         ;; `extension contains <ext> named <slice>`: the slice is typed Extension, profiled by that
         ;; extension's canonical (a local Extension's url, an alias, or a url as written)
         (define ext-profile
           (and ext? (contains-item-named-as it)
                (let* ([n (resolve-alias (contains-item-name it) (current-aliases))]
                       [local (hash-ref (current-profiles) n #f)])
                  (cond [local (local-profile-url local)]
                        [(regexp-match? #rx"^https?:" n) n]
                        [else (external-canonical "StructureDefinition" n)]))))
         (upd elems order sub-id rpath sub-name
              (lambda (h)
                (let ([h (apply-card (apply-flags h (contains-item-flags it)) (contains-item-card it))])
                  (if (and ext-profile (not (hash-has-key? h 'type)))
                      (hash-set h 'type (list (hasheq 'code "Extension" 'profile (list ext-profile))))
                      h)))))]
      [_ (values elems order)]))))
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
     ;; walk the path with the schema's types so a type-specific choice name (`valueQuantity`) becomes
     ;; the type slice SUSHI emits: id `value[x]:valueQuantity`, path `value[x]`, sliceName valueQuantity
     (define-values (id-segs path-segs last-slice)
       (for/fold ([ids '()] [paths '()] [slice #f] [type (string->symbol base-type)]
                  #:result (values (reverse ids) (reverse paths) slice))
                 ([st (in-list steps)])
         (define name (string->symbol (fsh-step-name st)))
         (define cb (and type (null? (fsh-step-brackets st)) (choice-base-of type name)))
         (define-values (_k ftype) (if type (field-info type name) (values #f (vector 'unknown))))
         (define next (and (eq? (ftype-tag ftype) 'type) (ftype-type-name ftype)))
         (if cb
             (values (cons (format "~a[x]:~a" cb name) ids) (cons (format "~a[x]" cb) paths) (symbol->string name) next)
             (values (cons (id-seg st) ids) (cons (path-seg st) paths) (step-slice st) next))))
     (values (string-append base-type "." (string-join id-segs "."))
             (string-append base-type "." (string-join path-segs "."))
             last-slice)]))
;; narrowed-choice : elems id path slice -> id path slice
;; once the profile has narrowed `value[x]` to exactly one type (`only Quantity`), `valueQuantity`
;; addresses `value[x]` itself (SUSHI): no type slice
(define (narrowed-choice elems id path slice)
  (define segs (string-split id "."))
  (define-values (out last-collapsed?)
    (for/fold ([acc '()] [collapsed? #f] #:result (values (reverse acc) collapsed?)) ([seg (in-list segs)])
      (define m (regexp-match #px"^(.*\\[x\\]):([A-Za-z]+)$" seg))
      (define choice-id (and m (string-join (reverse (cons (cadr m) acc)) ".")))
      (define types (and choice-id (hash-ref (hash-ref elems choice-id (hash)) 'type #f)))
      (define base (and m (regexp-replace #rx"\\[x\\]$" (cadr m) "")))
      (if (and m (list? types) (= 1 (length types))
               (string-ci=? (string-append base (hash-ref (car types) 'code "")) (caddr m)))
          (values (cons (cadr m) acc) #t)
          (values (cons seg acc) #f))))
  (if (equal? out segs)
      (values id path slice)
      (values (string-join out ".") path (if last-collapsed? #f slice))))
;; choice-base-of : type member -> "value" when `member` (valueQuantity) is an expanded choice
;; member of `type` (searching the type's generated ancestors), else #f
(define (choice-base-of type member)
  (let loop ([t type])
    (define sch (and t (type-schema-of t)))
    (cond [(not sch) #f]
          [(for/or ([f (in-list (type-schema-fields sch))] #:when (eq? (schema-field-name f) member))
             (schema-field-choice-base f))
           => symbol->string]
          [else (loop (type-schema-parent sch))])))
;; choice-slice-defaults : element base-type choice-path slice-name -> element with min/max/type
(define (choice-slice-defaults h base-type path slice)
  (define codes (or (base-type-codes base-type path) '()))
  (define base-prefix (regexp-replace #rx"\\[x\\]$" (last (string-split path ".")) ""))
  (define suffix (and slice (> (string-length slice) (string-length base-prefix)) (substring slice (string-length base-prefix))))
  ;; the slice name's type suffix matches one of the base types (Quantity; dateTime for DateTime)
  (define code (and suffix (for/or ([c (in-list codes)]) (and (string-ci=? c suffix) c))))
  (define bmax (base-max base-type path))
  (hash-set* h 'min 0 'max (or bmax "1") 'type (and code (list (hasheq 'code code)))))
(define (base-element base-type path)
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define els (and (hash? sd) (let ([sn (hash-ref sd 'snapshot #f)]) (and (hash? sn) (hash-ref sn 'element #f)))))
  (and (list? els) (findf (lambda (e) (equal? (hash-ref e 'path #f) path)) els)))
(define (base-max base-type path) (let ([e (base-element base-type path)]) (and e (hash-ref e 'max #f))))
;; base-type-codes : base-type element-path -> (listof type code) | #f   from the base snapshot
(define (base-type-codes base-type path)
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define els (and (hash? sd) (let ([sn (hash-ref sd 'snapshot #f)]) (and (hash? sn) (hash-ref sn 'element #f)))))
  (define el (and (list? els) (findf (lambda (e) (equal? (hash-ref e 'path #f) path)) els)))
  (and el (for/list ([t (in-list (hash-ref el 'type '()))]) (hash-ref t 'code #f))))
;; the type-slicing a choice element gets when one of its types is constrained by name (SUSHI)
(define type-slicing (hasheq 'discriminator (list (hasheq 'type "type" 'path "$this")) 'ordered #f 'rules "open"))
(define url-slicing (hasheq 'discriminator (list (hasheq 'type "value" 'path "url")) 'ordered #f 'rules "open"))
;; ensure-choice-parents : elems order id -> elems order   for each `x[x]:name` segment in `id`, the
;; `x[x]` element exists and carries the type slicing
(define (ensure-choice-parents elems order id)
  (for/fold ([elems elems] [order order]) ([m (in-list (regexp-match-positions* #px"\\[x\\]:" id))])
    (define pid (substring id 0 (+ (car m) 3)))
    (define ppath (regexp-replace* #px":[^.]+" pid ""))
    (upd elems order pid ppath #f (lambda (h) (if (hash-has-key? h 'slicing) h (hash-set h 'slicing type-slicing))))))
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
             [(regexp-match? #px"^https?:" n)
              ;; a profile by url: its code is the profile's base type (core vitalsigns -> Observation)
              (define sd (and (sd-index-available?) (resolve-canonical-raw "StructureDefinition" n)))
              (hasheq 'code (or (and sd (hash-ref sd 'type #f)) (last-url-seg n)) 'profile (list n))]
             [(and (sd-index-available?) (not (type-registered? (string->symbol n)))
                   (resolve-canonical-raw "StructureDefinition" n))
              => (lambda (sd) (hasheq 'code (hash-ref sd 'type) 'profile (list (hash-ref sd 'url))))]
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
;; ---- Extension auto-scaffolding (SUSHI-style) ----------------------------------------------
;; a top-level `extension` (type-characteristics can-bind) + a `context` of Element
(define (add-extension-header sd [default-context? #t])
  (hash-set* sd
             ;; the default context (Element) only when the Extension sets none itself (`^context[+]…`)
             'context (if default-context? (list (hasheq 'type "element" 'expression "Element")) (hash-ref sd 'context #f))
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
