#lang racket/base
;;; sd.rkt — Profile / Extension -> StructureDefinition.differential, Logical / Resource ->
;;; StructureDefinition (specialization), extension auto-scaffolding (SUSHI-style), Mapping and
;;; Invariant; `element-loc` maps FSH paths to ElementDefinition ids.
(require racket/match
         racket/string
         racket/contract
         (only-in racket/list append-map make-list last filter-map remove-duplicates index-of drop-right take drop)
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
(provide
 (contract-out
  ;; a Profile / Extension -> its StructureDefinition (differential), the Invariants by name
  [compile-profile (->* ((or/c fsh-profile? fsh-extension?)) (hash?) hash?)]
  ;; a Logical / Resource -> its StructureDefinition (specialization)
  [compile-logical-resource (-> (or/c fsh-logical? fsh-resource?) hash?)]
  ;; an Invariant -> the constraint source compile-profile reads
  [compile-invariant (-> fsh-invariant? hash?)]
  ;; a Mapping injected into its Source StructureDefinition among the compiled SDs
  [apply-mapping (-> list? fsh-mapping? list?)]
  ;; the path an element rule addresses, #f for other rules
  [rule-path-of (-> any/c (or/c string? #f))]
  ;; "1..*" -> 1 ; the low end of a cardinality
  [card-lo (-> string? exact-nonnegative-integer?)]))
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
  (define parent (strip-core-version (resolve-alias (or (assoc-ref meta 'parent) (if ext? "Extension" "DomainResource")) (current-aliases))))
  ;; a Parent defined in THIS document (by name, id or url): its canonical is local, and the
  ;; constrained type is found by following the Parent chain down to a base resource type
  (define local-parent (hash-ref (current-profiles) parent #f))
  (define base-type (if ext? "Extension" (instance-base parent (current-profiles))))
  (define parent-url (cond [local-parent (string-append (canonical-base) "/StructureDefinition/" (cdr local-parent))]
                           [(external-sd parent) => (lambda (sd) (hash-ref sd 'url))]
                           [else (sd-url parent)]))
  (define id (or (assoc-ref meta 'id) name))
  (define self-url (string-append (canonical-base) "/StructureDefinition/" id))
  ;; the nearest Parent in the packages (a local chain ends somewhere): its snapshot types inform `only`
  (define parent-sd (let loop ([n parent] [fuel 16])
                      (cond [(<= fuel 0) #f]
                            [(hash-ref (current-profiles) n #f) => (lambda (p) (loop (car p) (sub1 fuel)))]
                            [else (external-sd n)])))
  (define (parent-types path)
    (define els (and parent-sd (let ([sn (hash-ref parent-sd 'snapshot #f)]) (and (hash? sn) (hash-ref sn 'element '())))))
    (define el (and (list? els) (findf (lambda (e) (equal? (hash-ref e 'path #f) path)) els)))
    (if el (hash-ref el 'type '()) '()))
  ;; the Parent's elements (a local chain over the packages' snapshots, a base type's own snapshot):
  ;; `extension[us-core-birthsex]` names the slice typed with that extension; a caret into an
  ;; element's `binding`/`extension` edits what the Parent's element already carries
  (define parent-elements (profile-elements parent))
  (define slice-declarations (make-hash))
  (define-values (elems00 order00)
    (parameterize ([current-slice-declarations slice-declarations])
      (fold-sd-rules base-type (rewrite-extension-slices rules parent-elements base-type) invariants self-url
                     #:parent-types parent-types #:parent-element (lambda (id) (hash-ref parent-elements id #f)))))
  (define-values (elems0 order0) (let-values ([(e o) (add-slice-parents elems00 order00)]) (copy-slice-children e o)))
  ;; SUSHI writes what CHANGES: a property equal to the Parent's element's is not in the
  ;; differential, and an element left with nothing but its id and path is not either (a slice
  ;; stays, bare, so its children have a home)
  (define-values (elems1 order1) (drop-unchanged elems0 order0 (lambda (id) (hash-ref parent-elements id #f))))
  (define-values (elems order)
    (if ext? (add-extension-scaffold elems1 order1 self-url meta) (values elems1 order1)))
  (define diff (parameterize ([current-slice-declarations slice-declarations])
                 (snapshot-order base-type (for/list ([k (in-list (reverse order))]) (hash-ref elems k)))))
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
;; drop-unchanged : elems order (id -> element|#f) -> (values elems order)   each element minus the
;; properties its Parent's element already has with the same value; an element reduced to id/path
;; (no sliceName) is dropped with its place in `order`
(define (drop-unchanged elems order parent-element)
  (define trimmed
    (for/hash ([(id e) (in-hash elems)])
      (define pe (parent-element id))
      (values id
              (if (hash? pe)
                  (for/hash ([(k v) (in-hash e)]
                             #:unless (and (not (memq k '(id path sliceName))) (equal? v (hash-ref pe k 'none))))
                    (values k v))
                  e))))
  (define (bare? e) (and (not (hash-ref e 'sliceName #f))
                         (for/and ([k (in-hash-keys e)]) (memq k '(id path)))))
  (define kept (for/hash ([(id e) (in-hash trimmed)] #:unless (bare? e)) (values id e)))
  (values kept (filter (lambda (id) (hash-has-key? kept id)) order)))
;; strip-core-version : string -> string   a core type pinned to its version (`Parent:
;; Observation|4.0.1`, as a published IG's baseDefinition reads) is the type itself
(define (strip-core-version p)
  (match (regexp-match #px"^([^|]+)[|][^|]+$" p)
    [(list _ n) (if (type-registered? (string->symbol n)) n p)]
    [_ p]))
;; ---- Logical / Resource -> StructureDefinition (specialization, addElement) ----------------
(define (compile-logical-resource e)
  (define logical? (fsh-logical? e))
  (define-values (name meta rules)
    (if logical? (values (fsh-logical-name e) (fsh-logical-metadata e) (fsh-logical-rules e))
        (values (fsh-resource-name e) (fsh-resource-metadata e) (fsh-resource-rules e))))
  (define parent (or (assoc-ref meta 'parent) (if logical? "Base" "DomainResource")))
  (define id (or (assoc-ref meta 'id) name))
  (define self-url (string-append (canonical-base) "/StructureDefinition/" id))
  ;; `Characteristics: #can-be-target, #has-size`: one type-characteristics extension per code —
  ;; after the ones a Parent of this document carries (SUSHI starts from the parent's definition, so
  ;; IPS's IPSSectionsLM has its Document parent's can-be-target and then its own)
  (define parent-sd (let ([u (hash-ref (current-logicals) (resolve-alias parent (current-aliases)) #f)]) (and u ((current-profile-sd) u))))
  (define inherited-characteristics
    (if (hash? parent-sd)
        (filter (lambda (e) (equal? (hash-ref e 'url #f) "http://hl7.org/fhir/StructureDefinition/structuredefinition-type-characteristics"))
                (let ([x (hash-ref parent-sd 'extension '())]) (if (list? x) x '())))
        '()))
  (define characteristics
    (append inherited-characteristics
            (for/list ([c (in-list (or (assoc-ref meta 'characteristics) '()))])
              (hasheq 'url "http://hl7.org/fhir/StructureDefinition/structuredefinition-type-characteristics"
                      'valueCode (strip# (regexp-replace #px"[\\s,]+$" c ""))))))
  ;; the element paths of a logical/custom resource root at the entity id, not a FHIR type
  (define root (let ([short (or (assoc-ref meta 'title) name)])
                 (drop-false (hasheq 'id id 'path id 'short short 'definition (or (assoc-ref meta 'description) short)))))
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
              ;; a Parent that is a logical model of this document is named by its url
              'baseDefinition (or (hash-ref (current-logicals) (resolve-alias parent (current-aliases)) #f) (sd-url parent))
              'derivation "specialization"
              'extension (and (pair? characteristics) characteristics)
              'differential (hasheq 'element diff)))
     'abstract #f)
    rules)))
;; expand-flag-paths : rules -> rules   `* a and b and c MS` is one rule over three paths: one per path
(define (expand-flag-paths rules)
  (append-map (lambda (r)
                (match r
                  [(rule-flag p (? pair? extra) flags) (cons (rule-flag p '() flags) (for/list ([q (in-list extra)]) (rule-flag q '() flags)))]
                  [_ (list r)]))
              rules))
;; map-rule-path : (path -> path) rule -> rule   the rule with its path(s) mapped
(define (map-rule-path f r)
  (define (j p) (if (string? p) (f p) p))
  (match r
    [(rule-card p c fl) (rule-card (j p) c fl)]
    [(rule-flag p ps fl) (rule-flag (j p) (map j ps) fl)]
    [(rule-binding p v st) (rule-binding (j p) v st)]
    [(rule-assign p v x) (rule-assign (j p) v x)]
    [(rule-contains p items) (rule-contains (j p) items)]
    [(rule-only p ts) (rule-only (j p) ts)]
    [(rule-obeys p is) (rule-obeys (j p) is)]
    [(rule-caret p cp v) (rule-caret (j p) cp v)]
    [(rule-insert p ref) (rule-insert (j p) ref)]
    [(rule-path p) (rule-path (j p))]
    [(rule-add-element p c fl ts sh d) (rule-add-element (j p) c fl ts sh d)]
    [_ r]))
;; rewrite-extension-slices : rules parent-elements base-type -> rules   a leading
;; `extension[Name]` whose Name is not one of the Parent's slices but names an Extension (an alias,
;; a url, a profile) means the Parent's slice typed with it: `extension[us-core-birthsex] MS` is
;; the slice `birthsex` of US Core's Patient
(define (rewrite-extension-slices rules parent-elements base-type)
  (define (slice-for field name)
    (cond
      [(hash-has-key? parent-elements (string-append base-type "." field ":" name)) name]
      [(extension-url-of name)
       => (lambda (url)
            (or (for/or ([(id el) (in-hash parent-elements)])
                  (define m (regexp-match (pregexp (string-append "^" (regexp-quote (string-append base-type "." field ":")) "([^.]+)$")) id))
                  (and m (let ([ts (hash-ref el 'type '())])
                           (and (pair? ts) (member url (let ([pr (hash-ref (car ts) 'profile '())]) (if (list? pr) pr '()))) (cadr m)))))
                name))]
      [else name]))
  (define (fix p)
    (regexp-replace #px"^((?:modifierE|e)xtension)\\[([^\\]]+)\\]" p
                    (lambda (all field name) (string-append field "[" (slice-for field name) "]"))))
  (if (hash-empty? parent-elements) rules (map (lambda (r) (map-rule-path fix r)) rules)))
;; add-slice-parents : elems order -> (values elems order)   a slice whose children the rules
;; constrain is in the differential itself (SUSHI puts it there, bare but for its sliceName, as the
;; IG Publisher needs it to place the children)
(define (add-slice-parents elems order)
  (for*/fold ([es elems] [os order]) ([id (in-list (hash-keys elems))]
                                       [anc (in-list (slice-ancestors id))] #:unless (hash-has-key? es anc))
    (define segs (string-split anc "."))
    (define path (string-join (map (lambda (sg) (regexp-replace #rx":.*$" sg "")) segs) "."))
    (define slice (cadr (regexp-match #rx":(.*)$" (last segs))))
    (values (hash-set es anc (hasheq 'id anc 'path path 'sliceName slice)) (cons anc os))))
;; copy-slice-children : elems order -> (values elems order)   a named slice of X that the rules
;; constrain below (`section[sectionProblems].title`) carries copies of X's own sliced children
;; (`section.extension:section-note`, with their content): SUSHI unfolds the slice's children from
;; the element as constrained so far, and the copies have no original to equal
(define (copy-slice-children elems order)
  (define ids (hash-keys elems))
  (define (child-slice-of? x id)                        ; `X.name:slice`, directly under X
    (define m (regexp-match (pregexp (string-append "^" (regexp-quote x) "\\.([^.:]+):([^.]+)$")) id))
    (and m (cdr m)))
  (for*/fold ([es elems] [os order])
             ([sid (in-list ids)]
              #:when (and (regexp-match? #rx":[^.]+$" sid)                       ; a slice element …
                          (ormap (lambda (id) (string-prefix? id (string-append sid "."))) ids))   ; … with children
              [x (in-value (regexp-replace #rx":[^.]+$" sid ""))]
              [cid (in-list ids)] [m (in-value (child-slice-of? x cid))] #:when m
              [copy-id (in-value (string-append sid "." (car m) ":" (cadr m)))]
              #:unless (hash-has-key? es copy-id))
    (define src (hash-ref elems cid))
    (values (hash-set es copy-id (hash-set* src 'id copy-id 'path (hash-ref src 'path)))
            (cons copy-id os))))
;; slice-ancestors : element-id -> ids   its proper prefixes (at `.`) that end in a slice
(define (slice-ancestors id)
  (define segs (string-split id "."))
  (for/list ([n (in-range 1 (length segs))] #:when (regexp-match? #rx":" (list-ref segs (sub1 n))))
    (string-join (take segs n) ".")))
;; fold SD rules into an id-keyed differential-element accumulator (shared by profile/ext/logical)
;; parent-types : path -> (listof type) — the Parent's snapshot element at that path, for `only`
;; seed-caret-target : element caret-path parent-element|#f -> element
;; `^binding.extension…` / `^extension[url][+]…` on an element whose Parent's element already has a
;; binding / extensions: the caret EDITS the inherited value (SUSHI starts from the parent's element),
;; so the differential carries it whole — IPS's ImagingStudy keeps the base's display-hint ahead of
;; its obligations, and its procedureCode binding keeps the base's strength and value set (without
;; the core package's bindingName and explicit-type-name annotations, which SUSHI does not carry:
;; IPS's Composition.section has its obligations alone)
(define (seed-caret-target h cp pe)
  (define key (cond [(regexp-match? #px"^\\^?binding([.\\[]|$)" cp) 'binding]
                    [(regexp-match? #px"^\\^?extension([.\\[]|$)" cp) 'extension]
                    [else #f]))
  (define inherited (and key pe (hash? pe) (hash-ref pe key #f)))
  (define (without-core-hints exts)                       ; the core package's own annotations SUSHI drops
    (filter (lambda (e) (not (member (hash-ref e 'url #f) '("http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName"
                                                            "http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name"))))
            (if (list? exts) exts '())))
  (cond
    [(or (not inherited) (hash-has-key? h key)) h]
    [(eq? key 'binding)
     (define exts (without-core-hints (hash-ref inherited 'extension '())))
     (hash-set h 'binding (if (null? exts) (hash-remove inherited 'extension) (hash-set inherited 'extension exts)))]
    [else (define exts (without-core-hints inherited))
          (if (null? exts) h (hash-set h key exts))]))
(define (fold-sd-rules base-type rules invariants self-url #:parent-types [parent-types (lambda (_) '())]
                       #:parent-element [parent-element (lambda (_) #f)])
  (define (constraint-of key)
    (define inv (hash-ref invariants key #f))
    (drop-false (hasheq 'key key
                        'severity (and inv (hash-ref inv 'severity #f))
                        'human (and inv (hash-ref inv 'human #f))
                        'expression (and inv (hash-ref inv 'expression #f))
                        'xpath (and inv (hash-ref inv 'xpath #f))
                        'source self-url)))
  (define element-counters (make-hash))      ; element id -> soft-index counters for its carets
  (for/fold ([elems (hash)] [order '()]) ([r (in-list (expand-flag-paths rules))])
    (define p (rule-path-of r))
    (define-values (rid rpath rslice)
      (let-values ([(id path slice) (if (string? p) (element-loc base-type p) (values #f #f #f))])
        (if id (narrowed-choice elems id path slice) (values id path slice))))
    (define-values (elems* order*)
      (if (and rid (regexp-match? #px"\\[x\\]:[^.]+$" rid))
          (let-values ([(e o) (ensure-choice-parents elems order rid)])
            ;; a type slice SUSHI creates restricts the choice to that one type: min 0, the base max
            (if (hash-has-key? e rid)
                (values e o)
                (upd e o rid rpath rslice
                     (lambda (h) (choice-slice-defaults h base-type rpath rslice)))))
          (values elems order)))
    (let ([elems elems*] [order order*])
    (match r
      [(rule-card p card flags)
       (let-values ([(e o) (upd elems order rid rpath rslice (lambda (h) (apply-card (apply-flags h flags) card)))])
         ;; `valueIdentifier 1..`: the choice element value[x] itself becomes required too (SUSHI)
         (define m (regexp-match #px"^(.*\\[x\\]):[A-Za-z]+$" rid))
         (if (and m (> (card-lo card) 0) (hash-has-key? e (cadr m)) (not (hash-has-key? (hash-ref e (cadr m)) 'min)))
             (values (hash-update e (cadr m) (lambda (h) (hash-set h 'min (card-lo card)))) o)
             (values e o)))]
      ;; a flag the Parent's element already carries changes nothing, and SUSHI writes only what
      ;; changes: `* status MS` under US Core's observation (status MS there) adds no element
      [(rule-flag p _ flags)
       (define pe (parent-element rid))
       (define already (and (hash? pe) (not (hash-has-key? elems rid))
                            (for/and ([f (in-list flags)])
                              (cond [(eq? f 'MS) (eq? (hash-ref pe 'mustSupport #f) #t)]
                                    [(eq? f 'SU) (eq? (hash-ref pe 'isSummary #f) #t)]
                                    [(eq? f mod-flag) (eq? (hash-ref pe 'isModifier #f) #t)]
                                    [else #t]))))
       (if already (values elems order) (upd elems order rid rpath rslice (lambda (h) (apply-flags h flags))))]
      ;; `from VS` without a strength is a required binding (FSH's default)
      [(rule-binding p vs str)  (upd elems order rid rpath rslice (lambda (h) (hash-set h 'binding (drop-false (hasheq 'strength (if str (symbol->string str) "required") 'valueSet (and vs (valueset-url vs)))))))]
      [(rule-only p types)
       ;; types in the base element's order (SUSHI), unknown ones after
       (define base-codes (or (base-type-codes base-type rpath) '()))
       (define (rank t) (or (index-of base-codes (hash-ref t 'code)) 1000))
       ;; a kept type carries the Parent's extension on it (US Core's type-level must-support), as
       ;; SUSHI narrows the inherited type list rather than writing a new one
       (define inherited (parent-types rpath))
       (define (inherit t)
         (define pt (findf (lambda (x) (equal? (hash-ref x 'code #f) (hash-ref t 'code #f))) inherited))
         (if (and pt (hash-ref pt 'extension #f) (not (hash-ref t 'extension #f))) (hash-set t 'extension (hash-ref pt 'extension)) t))
       (define new-types (sort (map (compose inherit type->jsexpr) types) < #:key rank))
       ;; an `only` naming exactly the base element's types narrows nothing: SUSHI emits no element
       (if (and (not (hash-has-key? elems rid)) (equal? (base-type-codes base-type rpath) (map (lambda (t) (hash-ref t 'code)) new-types))
                (andmap (lambda (t) (= (hash-count t) 1)) new-types))
           (values elems order)
           (upd elems order rid rpath rslice (lambda (h) (hash-set h 'type new-types))))]
      [(rule-assign p v exact?)
       (let-values ([(e o) (upd elems order rid rpath rslice (lambda (h) (apply-pattern h base-type p (resolve v (current-aliases)) exact?)))])
         (values (require-discriminator e rid base-type) o))]
      [(rule-caret p cp v)      (if p (upd elems order rid rpath rslice
                                           (lambda (h0)
                                             (typed-caret-set (seed-caret-target h0 cp (parent-element rid)) cp v 'ElementDefinition
                                                              (hash-ref! element-counters rid make-hash))))
                                    (values elems order))]
      [(rule-obeys p invs)      (let-values ([(oid opath oslice) (if p (values rid rpath rslice) (values base-type base-type #f))])
                                  ;; several `obeys` on one element accumulate (each key once, first wins)
                                  (upd elems order oid opath oslice
                                       (lambda (h) (define have (hash-ref h 'constraint '()))
                                         (hash-set h 'constraint
                                                   (append have (for/list ([k (in-list invs)]
                                                                           #:unless (member k (map (lambda (c) (hash-ref c 'key #f)) have)))
                                                                  (constraint-of k)))))))]
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
         ;; the declaration order of the slices of this path, for the differential's order
         (let ([decl (current-slice-declarations)])
           (when (and decl (not (hash-has-key? decl (cons rid sn))))
             (hash-set! decl (cons rid sn) (hash-count decl))))
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
(define (base-element base-type path [fuel 8])
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define els (and (hash? sd) (let ([sn (hash-ref sd 'snapshot #f)]) (and (hash? sn) (hash-ref sn 'element #f)))))
  (define (at p) (and (list? els) (findf (lambda (e) (equal? (hash-ref e 'path #f) p)) els)))
  (or (at path)
      ;; below a complex element the snapshot stops (`Extension.extension.value[x]`): dive through
      ;; its one datatype
      (and (> fuel 0)
           (let ([segs (string-split path ".")])
             (for/or ([n (in-range (sub1 (length segs)) 1 -1)])
               (define prefix (string-join (take segs n) "."))
               (define el (at prefix))
               (define codes (and el (hash-ref el 'type '())))
               (and (list? codes) (= 1 (length codes))
                    (let ([c (hash-ref (car codes) 'code #f)])
                      (and (string? c)
                           (base-element c (string-join (cons c (drop segs n)) ".") (sub1 fuel))))))))))
(define (base-max base-type path) (let ([e (base-element base-type path)]) (and e (hash-ref e 'max #f))))
;; base-type-codes : base-type element-path -> (listof type code) | #f   from the base snapshot
(define (base-type-codes base-type path)
  (define el (base-element base-type path))
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
;; require-discriminator : elems id -> elems   a value assigned to a slice's value/pattern
;; discriminator element makes that element required (SUSHI: the slice is told apart by it, so an
;; instance of the slice must carry it): `identifier[displayName].use = #usual` under an `identifier`
;; sliced by `use` gives `identifier:displayName.use` min 1
(define (require-discriminator elems id base-type)
  (match (regexp-match #px"^(.*):[^.:/]+[.]([^:]+)$" id)
    [(list _ sliced rel)
     (define disc (let ([sl (hash-ref (hash-ref elems sliced (hash)) 'slicing #f)]) (and (hash? sl) (hash-ref sl 'discriminator '()))))
     ;; only an element the base leaves optional gets the min written (SUSHI writes what changes)
     (define base-min (let ([b (base-element base-type (hash-ref (hash-ref elems id (hash)) 'path ""))]) (and b (hash-ref b 'min #f))))
     (if (and (list? disc)
              (for/or ([d (in-list disc)])
                (and (hash? d) (member (hash-ref d 'type #f) '("value" "pattern")) (equal? (hash-ref d 'path #f) rel)))
              (equal? base-min 0)
              (not (hash-has-key? (hash-ref elems id (hash)) 'min)))
         (hash-update elems id (lambda (h) (hash-set h 'min 1)))
         elems)]
    [#f elems]))
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
  (match (regexp-match #px"^(Reference|Canonical|CodeableReference)\\s*\\((.*)\\)$" t)
    [(list _ "Canonical" inner) (hasheq 'code "canonical" 'targetProfile (target-urls inner))]
    [(list _ kind inner) (hasheq 'code kind 'targetProfile (target-urls inner))]
    [_ (define n (resolve-alias t (current-aliases)))
       (cond [(hash-ref (current-logicals) n #f) => (lambda (url) (hasheq 'code url))]   ; a logical type is its url
             [(hash-ref (current-profiles) n #f)
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
;; target-urls : "A or B|1.0" -> urls   each target a local profile, a profile of the packages by name
;; (USCorePatientProfile), an alias or a url; a `|version` pin stays on the canonical (SUSHI)
(define (target-urls inner)
  (for/list ([s (in-list (regexp-split #px"\\s+or\\s+" (string-trim inner)))])
    (match-define (list _ n0 ver) (regexp-match #px"^([^|]*)(\\|.*)?$" (string-trim s)))
    (define n (resolve-alias (string-trim n0) (current-aliases)))
    (string-append
     (cond [(hash-ref (current-profiles) n #f) => local-profile-url]
           [(and (sd-index-available?) (not (regexp-match? #px"^https?:" n)) (not (type-registered? (string->symbol n)))
                 (resolve-canonical-raw "StructureDefinition" n))
            => (lambda (sd) (hash-ref sd 'url))]
           [else (sd-url n)])
     (or ver ""))))
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
     ;; per inline sub-extension slice: forbid nested extension, fix its url to the slice name
     ;; (a slice typed with another Extension gets neither: that extension defines them)
     (append-map
      (lambda (sn)
        (define slice-el (hash-ref elems (string-append "Extension.extension:" sn) (hash)))
        (define typed? (for/or ([t (in-list (hash-ref slice-el 'type '()))]) (pair? (hash-ref t 'profile '()))))
        (if typed? '()
            (list (lambda (es os) (upd es os (format "Extension.extension:~a.extension" sn)
                                       "Extension.extension.extension" #f (lambda (h) (hash-set h 'max "0"))))
                  (lambda (es os) (upd es os (format "Extension.extension:~a.url" sn)
                                       "Extension.extension.url" #f (lambda (h) (hash-set h 'fixedUri sn)))))))
      slice-names)
     ;; a complex extension carries no value
     (if complex?
         (list (lambda (es os) (upd es os "Extension.value[x]" "Extension.value[x]" #f (lambda (h) (hash-set h 'max "0")))))
         '())
     ;; the extension's own url is fixed to its canonical
     (list (lambda (es os) (upd es os "Extension.url" "Extension.url" #f
                               (lambda (h) (hash-set h 'fixedUri self-url)))))))
  (for/fold ([es elems] [os order]) ([step (in-list steps)]) (step es os)))
;; apply-card : element "lo..hi" -> element   a bound left out (`1..`, `..3`) stays as it was
(define (apply-card h card)
  (match (regexp-match #px"^([0-9]*)\\.\\.([0-9]*|\\*)$" card)
    [(list _ lo hi)
     (let* ([h (if (positive? (string-length lo)) (hash-set h 'min (string->number lo)) h)]
            [h (if (positive? (string-length hi)) (hash-set h 'max hi) h)])
       h)]
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
  ;; element ids start with the type name — for a logical model (whose type is its url) with its id
  (define base-type (if (equal? (hash-ref sd 'kind #f) "logical") (hash-ref sd 'id) (hash-ref sd 'type)))
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

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  ;; an assignment to a slice's value discriminator makes it required; another element is untouched
  (let ([elems (hash "B.identifier" (hasheq 'slicing (hasheq 'discriminator (list (hasheq 'type "value" 'path "use"))))
                     "B.identifier:displayName.use" (hasheq 'patternCode "usual")
                     "B.identifier:displayName.value" (hasheq 'mustSupport #t))])
    (define elems* (for/hash ([(k v) (in-hash elems)]) (values k (hash-set v 'path (regexp-replace* #rx":[^.]+" (regexp-replace #rx"^B" k "BodyStructure") "")))))
    (check-equal? (hash-ref (hash-ref (require-discriminator elems* "B.identifier:displayName.use" "BodyStructure") "B.identifier:displayName.use") 'min #f) 1)
    (check-false (hash-ref (hash-ref (require-discriminator elems* "B.identifier:displayName.value" "BodyStructure") "B.identifier:displayName.value") 'min #f))
    (check-equal? (require-discriminator elems* "B.description" "BodyStructure") elems*))
  ;; `* a and b MS` flags every path; `Reference (A or B)` with a space is a Reference type
  (define multi (compile-profile (fsh-profile "P" '((parent . "Observation"))
                                              (list (rule-flag "status" '("code" "subject") '(MS))
                                                    (rule-only "basedOn" '("Reference (ServiceRequest or CarePlan)"))))))
  (define (el id) (findf (lambda (e) (equal? (hash-ref e 'id) id)) (hash-ref (hash-ref multi 'differential) 'element)))
  (check-equal? (map (lambda (id) (hash-ref (el id) 'mustSupport #f)) '("Observation.status" "Observation.code" "Observation.subject")) '(#t #t #t))
  (check-equal? (hash-ref (car (hash-ref (el "Observation.basedOn") 'type)) 'code) "Reference")
  (check-equal? (hash-ref (car (hash-ref (el "Observation.basedOn") 'type)) 'targetProfile)
                '("http://hl7.org/fhir/StructureDefinition/ServiceRequest" "http://hl7.org/fhir/StructureDefinition/CarePlan"))
  ;; slice ancestors and the bare slice elements SUSHI writes for constrained children
  (check-equal? (slice-ancestors "DiagnosticReport.category:Genetics.coding") '("DiagnosticReport.category:Genetics"))
  (check-equal? (slice-ancestors "Observation.component:a.extension:b.url") '("Observation.component:a" "Observation.component:a.extension:b"))
  (check-equal? (slice-ancestors "Observation.code") '())
  (let-values ([(es os) (add-slice-parents (hash "DiagnosticReport.category:Genetics.coding" (hasheq 'min 1)) '("DiagnosticReport.category:Genetics.coding"))])
    (check-equal? (hash-ref es "DiagnosticReport.category:Genetics") (hasheq 'id "DiagnosticReport.category:Genetics" 'path "DiagnosticReport.category" 'sliceName "Genetics")))
  ;; a constrained slice carries copies of the sliced element's own slice children
  (let-values ([(es os) (copy-slice-children
                         (hash "Composition.section.extension:note" (hasheq 'id "Composition.section.extension:note" 'path "Composition.section.extension" 'sliceName "note" 'short "notes")
                               "Composition.section:problems" (hasheq 'id "Composition.section:problems" 'path "Composition.section" 'sliceName "problems")
                               "Composition.section:problems.title" (hasheq 'id "Composition.section:problems.title" 'path "Composition.section.title" 'min 1))
                         '())])
    (check-equal? (hash-ref es "Composition.section:problems.extension:note")
                  (hasheq 'id "Composition.section:problems.extension:note" 'path "Composition.section.extension" 'sliceName "note" 'short "notes"))
    (check-equal? (hash-count es) 4))
  ;; an extension slice named by the extension's url is the Parent's slice typed with it
  (define pe (hash "Patient.extension:birthsex" (hasheq 'type (list (hasheq 'code "Extension" 'profile '("http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex"))))))
  (check-equal? (rule-path-of (car (rewrite-extension-slices (list (rule-flag "extension[http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex]" '() '(MS))) pe "Patient")))
                "extension[birthsex]")
  (check-equal? (rule-path-of (car (rewrite-extension-slices (list (rule-card "extension[birthsex].value[x]" "1..1" '())) pe "Patient")))
                "extension[birthsex].value[x]")
  ;; cardinalities and rule paths
  (check-equal? (card-lo "1..*") 1) (check-equal? (card-lo "0..1") 0) (check-equal? (card-lo "..1") 0)
  (check-equal? (rule-path-of (rule-card "component" "1..*" '())) "component")
  (check-false (rule-path-of 'not-a-rule))
  ;; element-loc: an FSH path to the ElementDefinition id, path and slice name SUSHI would write
  (define (loc base p) (call-with-values (lambda () (element-loc base p)) list))
  (check-equal? (loc "Observation" ".") '("Observation" "Observation" #f))
  (check-equal? (loc "Observation" "component.code") '("Observation.component.code" "Observation.component.code" #f))
  (check-equal? (loc "Observation" "component[systolic].code")
                '("Observation.component:systolic.code" "Observation.component.code" #f))
  (check-equal? (loc "Observation" "component[systolic]")
                '("Observation.component:systolic" "Observation.component" "systolic"))
  ;; a type-specific choice name is the type slice
  (check-equal? (loc "Observation" "valueQuantity")
                '("Observation.value[x]:valueQuantity" "Observation.value[x]" "valueQuantity"))
  (check-equal? (loc "Observation" "value[x]") '("Observation.value[x]" "Observation.value[x]" #f)))
