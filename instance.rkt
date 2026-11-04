#lang racket/base
;;; instance.rkt — Instance -> a plain resource: schema-driven nesting (resolve.rkt), soft indices, named
;;; slices in their positions, the required values an InstanceOf profile implies (assign-required),
;;; meta.profile. `coerce`, `external-sd` and `instance-base` serve the StructureDefinition side too.
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
         "context.rkt")
(provide
 (contract-out
  ;; an Instance -> the resource JSON, given the aliases and the local profiles (name -> (parent . sd))
  [compile-instance (-> fsh-instance? hash? hash? hash?)]
  ;; the base resource type of an InstanceOf, through local profile chains
  [instance-base (->* (string? hash?) (exact-nonnegative-integer?) (or/c string? #f))]
  ;; a dependency package's profile by name, id or url (a constraint SD), when the SD index has it
  [external-sd (-> string? (or/c hash? #f))]
  ;; an assigned FSH value as the element's JSON, by the element's ftype
  [coerce (-> any/c any/c any/c)]))
;; ---- Instance -> a plain resource ----------------------------------------------------------
(define (compile-instance e aliases profiles)
  (define meta (fsh-instance-metadata e))
  ;; InstanceOf may be an Alias (`Alias: USCorePatient = http://…/us-core-patient`)
  (define type-str (resolve-alias (assoc-ref meta 'instanceOf) aliases))
  ;; resolve InstanceOf to a base resource type: a base type stays; a local profile -> its parent
  (define base (instance-base type-str profiles))
  (define t (string->symbol base))
  (define js (let* ([j (build-instance-json e t aliases)]
                    [j (if (type-registered? (string->symbol type-str))
                           j
                           (assign-required j (profile-elements type-str) (slice-positions e aliases)))])
               (definitional-url j (assoc-ref meta 'usage))))
  ;; stamp meta.profile when InstanceOf named a profile (not a base type), using the profile's id-URL
  (define prof (and (not (type-registered? (string->symbol type-str))) (hash-ref profiles type-str #f)))
  (cond
    [prof (inject-meta-profile js (string-append (canonical-base) "/StructureDefinition/" (cdr prof)))]
    ;; InstanceOf a dependency's profile: SUSHI stamps that profile's canonical
    [(external-sd type-str) => (lambda (sd) (inject-meta-profile js (hash-ref sd 'url)))]
    [else js]))
;; definitional-url : jsexpr usage -> jsexpr   a `Usage: #definition` instance of a canonical resource
;; (CapabilityStatement, OperationDefinition, …) gets SUSHI's url <canonical>/<Type>/<id>
(define (definitional-url js usage)
  (define type (hash-ref js 'resourceType))
  (if (and (equal? usage "#definition") (not (hash-has-key? js 'url)) (hash-ref js 'id #f) (has-element? type "url"))
      (hash-set js 'url (string-append (canonical-base) "/" type "/" (hash-ref js 'id)))
      js))
(define (has-element? type field)
  (define sd (and (sd-index-available?) (resolve-sd-raw type)))
  (and (hash? sd)
       (for/or ([e (in-list (hash-ref (hash-ref sd 'snapshot (hash)) 'element '()))])
         (equal? (hash-ref e 'id #f) (string-append type "." field)))))
;; follows the Parent chain (a profile of a profile of …) to the base resource type; bounded so a
;; cyclic Parent cannot loop
(define (instance-base type-str profiles [fuel 16])
  (cond [(type-registered? (string->symbol type-str)) type-str]
        [(and (> fuel 0) (hash-ref profiles type-str #f))
         => (lambda (p) (instance-base (let ([par (resolve-alias (car p) (current-aliases))])
                                        (if (or (hash-ref profiles par #f) (external-sd par)) par (last-url-seg par)))
                                       profiles (sub1 fuel)))]
        ;; a profile from a dependency package (US Core, genomics-reporting …): its constrained type
        [(external-sd type-str) => (lambda (sd) (hash-ref sd 'type type-str))]
        [else type-str]))
;; external-sd : name|id|url -> StructureDefinition jsexpr | #f   a PROFILE (not a base type) from the
;; packages in current-sd-packages — the IG's dependencies, then core
(define (external-sd n)
  (and (sd-index-available?)
       (not (type-registered? (string->symbol n)))
       (let ([sd (resolve-sd-raw n)])
         (and (hash? sd) (equal? (hash-ref sd 'derivation #f) "constraint") sd))))
;; slice-positions : instance aliases -> (hash (cons field index) -> slice-name)   where each
;; top-level named slice the instance mentions (`component[systolic]`) was placed
(define (slice-positions e aliases)
  (define assigns (for/list ([r (in-list (fsh-instance-rules e))] #:when (or (rule-assign? r) (rule-path? r))) r))
  (define of (let ([o (assoc-ref (fsh-instance-metadata e) 'instanceOf)]) (and o (resolve-alias o aliases))))
  (define orig (for/list ([r (in-list assigns)]) (fsh-path-steps (parse-fsh-path (if (rule-path? r) (rule-path-path r) (rule-assign-path r))))))
  (define resolved (resolve-soft-indices orig #:slice-order (if of ((current-slice-orders) of) (hash))))
  (for*/hash ([(o r) (in-parallel orig resolved)]
              #:when (and (pair? o) (pair? r))
              [b (in-list (fsh-step-brackets (car o)))] #:when (eq? (fsh-bracket-kind b) 'slice)
              [rb (in-list (fsh-step-brackets (car r)))] #:when (eq? (fsh-bracket-kind rb) 'index))
    (values (cons (fsh-step-name (car o)) (fsh-bracket-value rb)) (fsh-bracket-value b))))
(define (build-instance-json e resource-type aliases)
  ;; an instance of a logical model (registered with Element as its parent: no resourceType on the
  ;; wire, no id element unless the model declares one) is not stamped with an id
  (define logical? (let ([sch (type-schema-of resource-type)]) (and sch (eq? (type-schema-parent sch) 'Element))))
  (define base (if logical?
                   (hasheq 'resourceType (symbol->string resource-type))
                   (hasheq 'resourceType (symbol->string resource-type) 'id (fsh-instance-name e))))
  ;; path-only rules (`* extension[x][+]`, the context of indented rules) assign nothing but still
  ;; advance soft indices, so they take part in index resolution
  (define assigns (for/list ([r (in-list (fsh-instance-rules e))] #:when (or (rule-assign? r) (rule-path? r))) r))
  (define of (let ([o (assoc-ref (fsh-instance-metadata e) 'instanceOf)]) (and o (resolve-alias o aliases))))
  (define paths (resolve-soft-indices (for/list ([r (in-list assigns)]) (fsh-path-steps (parse-fsh-path (if (rule-path? r) (rule-path-path r) (rule-assign-path r)))))
                                      #:slice-order (if of ((current-slice-orders) of) (hash))))
  (for/fold ([js base]) ([r (in-list assigns)] [steps (in-list paths)] #:when (rule-assign? r))
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
  (define high (make-hash))                         ; key -> the highest index used so far
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
      (for ([n (in-list ordered)] [i (in-naturals)]) (hash-set! slices (cons key n) i) (hash-set! high key i))
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
                ;; a slice mentioned again reuses its index; a new one takes the next UNUSED
                ;; position (after re-mentioning an earlier slice the counter points back at it)
                (cond [(hash-ref slices sk #f) => (lambda (n) (hash-set! counters key n) (fsh-bracket 'index n))]
                      [else (define n (add1 (max (hash-ref counters key -1) (hash-ref high key -1))))
                            (hash-set! counters key n) (hash-set! slices sk n) (fsh-bracket 'index n)])]
               [else b])))
         (for ([b (in-list brackets)] #:when (eq? (fsh-bracket-kind b) 'index))
           (hash-update! high key (lambda (h) (max h (fsh-bracket-value b))) -1))
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
(define json-number-primitives '("integer" "decimal" "positiveInt" "unsignedInt" "boolean"))
(define (ftype->type ftype)
  (case (ftype-tag ftype) [(type) (ftype-type-name ftype)] [else 'object]))
;; coerce an FSH value to jsexpr given the target ftype
(define (coerce v ftype)
  (define tag (ftype-tag ftype))
  (match* (tag v)
    ;; `System#code` on a code-typed primitive keeps only the code (`* status = MedReqStatus#completed`)
    [('prim (struct fsh-code (c _))) (let-values ([(_sys code) (split-code c)]) code)]
    ;; a number assigned to a primitive whose JSON form is a string (date `2020`, string `42`)
    [('prim (? number? n)) #:when (not (member (ftype-prim-code ftype) json-number-primitives)) (number->string n)]
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
;; profile-elements : profile-name|id|url -> (hash element-id -> ElementDefinition jsexpr)
;; the constraints an Instance of that profile inherits: a dependency profile's (or the base type's)
;; snapshot, overlaid by each local profile's differential down the Parent chain (the child's keys win)
(define (profile-elements key [fuel 16])
  (define k (resolve-alias key (current-aliases)))
  (cond
    [(<= fuel 0) (hash)]
    ;; the chain ends at a base type: its snapshot supplies what the profiles do not restate (min)
    [(type-registered? (string->symbol k))
     (define sd (and (sd-index-available?) (resolve-sd-raw k)))
     (if (hash? sd)
         (for/hash ([e (in-list (hash-ref (hash-ref sd 'snapshot (hash)) 'element '()))] #:when (hash-ref e 'id #f))
           (values (hash-ref e 'id) e))
         (hash))]
    [((current-profile-sd) k)
     => (lambda (sd)
          (define parent (let ([p (hash-ref (current-profiles) k #f)]) (and p (car p))))
          (define inherited (if parent (profile-elements parent (sub1 fuel)) (hash)))
          (for/fold ([h inherited]) ([e (in-list (hash-ref (hash-ref sd 'differential (hash)) 'element '()))]
                                     #:when (hash-ref e 'id #f))
            (hash-update h (hash-ref e 'id) (lambda (old) (for/fold ([o old]) ([(kk v) (in-hash e)]) (hash-set o kk v))) e)))]
    [(external-sd k)
     => (lambda (sd)
          (for/hash ([e (in-list (hash-ref (hash-ref sd 'snapshot (hash)) 'element '()))] #:when (hash-ref e 'id #f))
            (values (hash-ref e 'id) e)))]
    [else (hash)]))
;; assign-required : jsexpr (hash id -> element) -> jsexpr
;; SUSHI assigns what the InstanceOf profile REQUIRES and the instance leaves out: a required element
;; (min >= 1) gets its `required-entry` (its fixed[x]/pattern[x], completed by its required children,
;; recursively); a required slice gets its entry appended unless an entry already carries it; a named
;; slice the instance uses is completed the same way (and gets its own pattern even when optional).
(define (assign-required js elements [positions (hash)])
  (assign-required* (fill-slice-children js elements positions) elements))
;; fill-slice-children : jsexpr elements positions -> jsexpr
;; a named slice the instance used gets the slice's pattern and its required children's values
(define (fill-slice-children js elements positions)
  (define type (hash-ref js 'resourceType))
  (for/fold ([js js]) ([(pos slice) (in-hash positions)])
    (define field (string->symbol (car pos)))
    (define lst (hash-ref js field #f))
    (cond
      [(not (and (list? lst) (< (cdr pos) (length lst)) (hash? (list-ref lst (cdr pos))))) js]
      [else
       (define slice-id (string-append type "." (car pos) ":" slice))
       (define slice-el (hash-ref elements slice-id #f))
       ;; an extension slice's url is the canonical of the Extension it is typed with
       (define ext-url (and slice-el (member (car pos) '("extension" "modifierExtension"))
                            (let* ([ts (hash-ref slice-el 'type '())]
                                   [pr (and (pair? ts) (hash-ref (car ts) 'profile '()))])
                              (and (pair? pr) (car pr)))))
       (define en (list-ref lst (cdr pos)))
       (define pinned (let ([v (and slice-el (pinned-value slice-el))]) (and (hash? v) v)))
       (define entry (merge-missing (merge-missing (if (and ext-url (not (hash-has-key? en 'url))) (hash-set en 'url ext-url) en)
                                                   (or pinned (hasheq)))
                                    (required-children elements slice-id)))
       (hash-set js field (for/list ([x (in-list lst)] [i (in-naturals)]) (if (= i (cdr pos)) entry x)))])))
;; pinned-value : element -> jsexpr|#f   its fixed[x]/pattern[x]
(define (pinned-value e)
  (for/or ([(k v) (in-hash e)]) (and (regexp-match? #rx"^(fixed|pattern)" (symbol->string k)) v)))
(define (required? e)
  (let ([mn (hash-ref e 'min 0)]) (and (number? mn) (>= mn 1))))
;; required-in? : elements id -> boolean   the element's min, else (inside a slice) the unsliced element's
(define (required-in? elements id)
  (define e (hash-ref elements id (hash)))
  (if (hash-has-key? e 'min)
      (required? e)
      (required? (hash-ref elements (regexp-replace* #rx":[^.]+" id "") (hash)))))
;; required-entry : elements id -> jsexpr|#f   the value SUSHI builds for a required element: its pin,
;; completed by its required children; #f when nothing below it is pinned
(define (required-entry elements id)
  (define own (pinned-value (hash-ref elements id (hash))))
  (define kids (required-children elements id))
  (cond [(and own (not (hash? own))) own]
        [(and (not own) (hash-empty? kids)) #f]
        [else (merge-missing (or own (hasheq)) kids)]))
;; required-children : elements id -> (hash field -> jsexpr)   the direct children of `id` that are
;; required and have a required-entry; a child's required named slices are its entries (in place of
;; the unsliced element's), a list when the child repeats
(define (required-children elements id)
  (define prefix (string-append id "."))
  (define groups                                     ; field -> (listof (cons slice|#f value))
    (for*/fold ([g (hash)]) ([(cid el) (in-hash elements)]
                             [rest (in-value (and (string-prefix? cid prefix) (substring cid (string-length prefix))))]
                             [m (in-value (and rest (regexp-match #px"^([A-Za-z]+)(?::([^.]+))?$" rest)))]
                             #:when (and m (required-in? elements cid) (not (member (cadr m) '("extension" "modifierExtension" "id"))))
                             [v (in-value (required-entry elements cid))] #:when v)
      (hash-update g (cadr m) (lambda (l) (cons (cons (caddr m) v) l)) '())))
  (for/hasheq ([(name vs) (in-hash groups)])
    (define sliced (map cdr (sort (filter car vs) string<? #:key car)))
    (define vals (if (pair? sliced) sliced (list (cdr (assf not vs)))))
    (values (string->symbol name) (if (repeats? elements (string-append id "." name)) vals (car vals)))))
(define (assf f l) (for/first ([p (in-list l)] #:when (f (car p))) p))
;; merge-missing : jsexpr jsexpr -> jsexpr   `a` with the keys only `b` has (objects, recursively)
(define (merge-missing a b)
  (if (and (hash? a) (hash? b))
      (for/fold ([a a]) ([(k v) (in-hash b)])
        (hash-set a k (if (hash-has-key? a k) (merge-missing (hash-ref a k) v) v)))
      a))
;; repeats? : elements id -> boolean   the element at `id` repeats: its max, else the base model's
(define (repeats? elements id)
  (define mx (let ([el (hash-ref elements (regexp-replace* #rx":[^.]+" id "") #f)]) (and el (hash-ref el 'max #f))))
  (cond [(equal? mx "*") #t]
        [(and (string? mx) (string->number mx)) => (lambda (n) (> n 1))]
        [else (path-repeats? (regexp-replace* #rx":[^.]+" id ""))]))
;; path-repeats? : "Type.a.b" -> boolean   via the registered tower
(define (path-repeats? path)
  (define steps (string-split path "."))
  (define parent (if (= (length steps) 2)
                     (car steps)
                     (element-fhir-type (string->symbol (car steps)) (string-join (drop-right (cdr steps) 1) "."))))
  (and parent (pair? (cdr steps))
       (let-values ([(k _) (field-info (string->symbol parent) (string->symbol (last steps)))]) (eq? k 'list))))
(define (assign-required* js elements)
  (define type (hash-ref js 'resourceType))
  (define rx (pregexp (string-append "^" (regexp-quote type) "[.]([A-Za-z]+)(?::([^.]+))?$")))
  (define (sliced-required? field)
    (define p (string-append type "." field ":"))
    (for/or ([(id e) (in-hash elements)]) (and (string-prefix? id p) (required? e) (required-entry elements id) #t)))
  (for/fold ([js js]) ([(id e) (in-hash elements)])
    (define m (regexp-match rx id))
    (define value (and m (required? e) (not (member (cadr m) '("extension" "modifierExtension" "id" "meta")))
                       (required-entry elements id)))
    (cond
      [(not value) js]
      [(caddr m)                                                ; a required slice
       (define field (string->symbol (cadr m)))
       (define cur (let ([v (hash-ref js field '())]) (if (list? v) v (list v))))
       (if (ormap (lambda (x) (json-subset? value x)) cur) js (hash-set js field (append cur (list value))))]
      [(hash-has-key? js (string->symbol (cadr m))) js]
      [(sliced-required? (cadr m)) js]                         ; its slices supply the entries
      [else
       (hash-set js (string->symbol (cadr m)) (if (repeats? elements id) (list value) value))])))
(define (json-subset? a b)
  (cond [(and (hash? a) (hash? b)) (for/and ([(k v) (in-hash a)]) (and (hash-has-key? b k) (json-subset? v (hash-ref b k))))]
        [(and (list? a) (list? b)) (for/and ([x (in-list a)]) (ormap (lambda (y) (json-subset? x y)) b))]
        [else (equal? a b)]))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  ;; coerce: what an assigned FSH value becomes in the element's JSON
  (check-equal? (coerce (fsh-code "http://loinc.org#8480-6" "Systolic") (vector 'type 'Coding))
                (hasheq 'system "http://loinc.org" 'code "8480-6" 'display "Systolic"))
  (check-equal? (coerce (fsh-code "#final" #f) (vector 'prim "code")) "final")
  (check-equal? (coerce (fsh-code "Sys#completed" #f) (vector 'prim "code")) "completed")
  (check-equal? (coerce 2020 (vector 'prim "date")) "2020")
  (check-equal? (coerce 42 (vector 'prim "integer")) 42)
  (check-equal? (coerce (fsh-quantity 120 "mm[Hg]" #f) (vector 'type 'Quantity))
                (hasheq 'value 120 'unit "mm[Hg]" 'code "mm[Hg]" 'system "http://unitsofmeasure.org"))
  (check-equal? (coerce (fsh-reference "Patient/1" "Ann") (vector 'type 'Reference)) (hasheq 'reference "Patient/1" 'display "Ann"))
  (check-equal? (coerce #t (vector 'prim "boolean")) #t)
  ;; instance-base: a base type stays, a local profile goes to its parent, chains end at the base
  (check-equal? (instance-base "Patient" (hash)) "Patient")
  (check-equal? (instance-base "MyPatient" (hash "MyPatient" (cons "Patient" "my-patient"))) "Patient")
  (check-equal? (instance-base "Deeper" (hash "Deeper" (cons "MyPatient" "deeper") "MyPatient" (cons "Patient" "my-patient"))) "Patient"))
