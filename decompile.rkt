#lang racket/base

;;; decompile.rkt — FHIR JSON -> FSH (the GoFSH / reverse direction), making the lang bidirectional.
;;;
;;; Instances are decompiled by a per-leaf jsexpr walk that emits `* path = rawvalue` for every
;;; primitive leaf (with explicit [i] array indices) — exactly lossless, since the forward compiler
;;; passes string/number/bool values straight through, so recompiling reproduces the original jsexpr.
;;; Profiles/ValueSets/CodeSystems emit idiomatic rules where recognised (cardinality, binding, only,
;;; concept, include/exclude) and fall back to caret-value rules for everything else (the lossless
;;; catch-all the forward compiler applies via caret-set). Adapted from the stroma prior art; the
;;; typed-node entry (`node->fsh`) uses this repo's `node->jsexpr`.

(require racket/match
         racket/list
         racket/string
         racket/contract
         "parser/ast.rkt"
         (only-in "print.rkt" print-doc)
         (only-in fhir/serialize/to-json node->jsexpr)
         (only-in fhir/model/node fhir?))

(provide
 (contract-out
  [fhir->fsh        (->* (hash?) () #:rest (listof hash?) string?)]  ; FHIR jsexpr(s) -> FSH document text
  [fhir->fsh-entity (-> hash? any/c)]                                ; one FHIR jsexpr -> an FSH entity AST
  [node->fsh        (->* (fhir?) () #:rest (listof fhir?) string?)]));; typed node(s) -> FSH text

;; FHIR jsexpr(s) -> FSH: decompile each resource, then synthesise the Invariant entities the SD
;; constraints reference (SUSHI keeps invariants only inside SDs; we regenerate them so recompiling
;; refills each constraint's severity/human/expression).
(define (fhir->fsh js . more)
  (define jss (cons js more))
  (print-doc (append (map fhir->fsh-entity jss) (synth-invariants jss))))
(define (node->fsh n . more)  (apply fhir->fsh (map node->jsexpr (cons n more))))

(define (synth-invariants jss)
  (define seen (make-hash))
  (for*/list ([j (in-list jss)]
              #:when (equal? (hash-ref j 'resourceType #f) "StructureDefinition")
              [elt (in-list (hash-ref* j 'differential 'element))]
              [c (in-list (hash-ref elt 'constraint '()))]
              #:unless (hash-has-key? seen (hash-ref c 'key)))
    (hash-set! seen (hash-ref c 'key) #t)
    (fsh-invariant (hash-ref c 'key)
                   (append (opt-meta c 'human 'description values)
                           (opt-meta c 'severity 'severity (lambda (s) (string-append "#" s)))
                           (opt-meta c 'expression 'expression values)
                           (opt-meta c 'xpath 'xpath values))
                   '())))

(define (opt-meta h k mk f) (let ([v (hash-ref h k #f)]) (if v (list (cons mk (f v))) '())))

(define (fhir->fsh-entity js)
  (case (and (hash? js) (hash-ref js 'resourceType #f))
    [("StructureDefinition") (decompile-sd js)]
    [("ValueSet")            (decompile-valueset js)]
    [("CodeSystem")          (decompile-codesystem js)]
    [else                    (decompile-instance js)]))

;; ---- Instance — per-leaf assignment walk (lossless) ----------------------------------------
;; a conformance resource (one the IG Publisher keeps in package/, not package/example/) is a
;; `#definition` instance; GoFSH does the same
(define definition-types
  '("CapabilityStatement" "OperationDefinition" "SearchParameter" "CompartmentDefinition" "ImplementationGuide"
    "StructureMap" "GraphDefinition" "ExampleScenario" "MessageDefinition" "NamingSystem" "ConceptMap"
    "TerminologyCapabilities" "Questionnaire" "ActivityDefinition" "PlanDefinition" "Measure" "Library"))
(define (decompile-instance js)
  (define type (hash-ref js 'resourceType))
  (define id (hash-ref js 'id "Example"))
  (define usage (if (member type definition-types) "#definition" "#example"))
  (fsh-instance (str id) (list (cons 'instanceOf type) (cons 'usage usage)) (walk js "" #t)))

(define (walk js path top?)
  (cond
    [(hash? js)
     ;; `resourceType` first: an embedded resource is typed by the first rule that names it (its
     ;; arrays nest right only once it is typed); `_x` rides with `x`, or alone (an absent value
     ;; carrying extensions) is written at `x`
     (define keys (sort (hash-keys js) symbol<?))
     (define ordered (if (memq 'resourceType keys) (cons 'resourceType (remq 'resourceType keys)) keys))
     (append*
      (for/list ([k (in-list ordered)]
                 #:unless (and top? (memq k '(resourceType id)))
                 #:unless (and (underscore? k) (hash-has-key? js (plain k))))
        (define here (extend path (symbol->string (if (underscore? k) (plain k) k))))
        (append (walk (hash-ref js k) here #f)
                (let ([sib (and (not (underscore? k)) (hash-ref js (underscore k) #f))]) (if sib (walk sib here #f) '())))))]
    [(list? js)
     (append* (for/list ([elt (in-list js)] [i (in-naturals)]) (walk elt (format "~a[~a]" path i) #f)))]
    [else (list (rule-assign path js #f))]))

(define (extend path seg) (if (= 0 (string-length path)) seg (string-append path "." seg)))
(define (underscore? k) (char=? (string-ref (symbol->string k) 0) #\_))
(define (underscore k) (string->symbol (string-append "_" (symbol->string k))))
(define (plain k) (string->symbol (substring (symbol->string k) 1)))
(define (str x) (if (string? x) x (format "~a" x)))

;; ---- ValueSet / CodeSystem -----------------------------------------------------------------
(define (decompile-valueset js)
  (define rules
    (append
     (append-map (lambda (inc) (vs-include->rules inc #t)) (hash-ref* js 'compose 'include))
     (append-map (lambda (exc) (vs-include->rules exc #f)) (hash-ref* js 'compose 'exclude))
     (caret-rest js (append publisher-only '(resourceType id title description compose name url status version)))))
  (fsh-valueset (entity-name js "ValueSet") (append (id-meta js) (sd-meta js)) rules))

;; an include/exclude component -> one FSH vs-component per concept, or a `codes from system X
;; [and valueset A and B] [where filters]` component (a version rides on the system: `X|1.2`)
(define (vs-include->rules inc include?)
  (define sys (let ([s (hash-ref inc 'system #f)] [v (hash-ref inc 'version #f)])
                (and s (if v (format "~a|~a" s v) s))))
  (define vss (let ([v (hash-ref inc 'valueSet '())]) (if (list? v) v '())))
  (define concepts (hash-ref inc 'concept '()))
  (define filters (let ([f (hash-ref inc 'filter '())]) (if (list? f) f '())))
  (define from-parts
    (append (if sys (list (list 'vsFromSystem "system" (list 'name sys))) '())
            (if (pair? vss) (list (list* 'vsFromValueset "valueset" (map (lambda (v) (list 'name v)) vss))) '())))
  (cond
    [(pair? concepts)
     (for/list ([c (in-list concepts)])
       (define disp (hash-ref c 'display #f))
       (define code-node (if disp
                             (list 'code (code-tok (format "~a#~a" (or sys "") (hash-ref c 'code))) disp)
                             (list 'code (code-tok (format "~a#~a" (or sys "") (hash-ref c 'code))))))
       (vs-component include? (list (list 'vsConceptComponent code-node))))]
    [(null? from-parts) '()]
    [else
     (list (vs-component include?
                         (list (list* 'vsFilterComponent "codes"
                                      (list* 'vsComponentFrom "from" (add-between from-parts "and"))
                                      (if (pair? filters)
                                          (list "where" (cons 'vsFilterList (map filter->node filters)))
                                          '())))))]))
;; filter->node : ValueSet filter -> vsFilterDefinition datum   `property op value`: a code for the
;; concept operators, true/false for exists, /re/ for regex, a string otherwise
(define (filter->node f)
  (define op (hash-ref f 'op "="))
  (define v (hash-ref f 'value ""))
  (define value
    (cond [(member op '("is-a" "is-not-a" "descendent-of" "generalizes" "child-of" "descendent-leaf"))
           (list 'code (code-tok (string-append "#" v)))]
          [(equal? op "exists") (equal? v "true")]
          [(equal? op "regex") (list 'regex v)]
          [else v]))
  (list 'vsFilterDefinition (list 'name (hash-ref f 'property "concept")) (list 'vsFilterOperator op) (list 'vsFilterValue value)))

(define (decompile-codesystem js)
  (define rules
    (append
     (concepts->rules (hash-ref js 'concept '()) '())
     (if (member (hash-ref js 'content "complete") '("complete" #f)) '() (list (rule-caret #f "^content" (hash-ref js 'content))))
     (caret-rest js (append publisher-only '(resourceType id title description concept name content url status version count)))))
  (fsh-codesystem (entity-name js "CodeSystem") (append (id-meta js) (sd-meta js)) rules))

;; a concept tree -> flat concept-rules carrying the ancestor code chain (`* #a #b "..."`)
(define (concepts->rules concepts ancestors)
  (append*
   (for/list ([c (in-list concepts)])
     (define codes (append ancestors (list (string-append "#" (hash-ref c 'code)))))
     (cons (concept-rule codes (hash-ref c 'display #f) (hash-ref c 'definition #f))
           (concepts->rules (hash-ref c 'concept '()) codes)))))

;; ---- StructureDefinition (Profile / Extension) — differential + caret catch-all ------------
(define (decompile-sd js)
  (cond
    ;; logical models & custom resources derive by specialization
    [(equal? (hash-ref js 'derivation #f) "specialization") (decompile-logical-resource js)]
    ;; extensions: emit minimal FSH, let the forward compiler regenerate the scaffold
    [(equal? (hash-ref js 'type #f) "Extension") (decompile-extension js)]
    [else (decompile-profile-extension js)]))

;; Logical / custom Resource -> addElement rules (element paths root at the entity id); what else
;; an element says (comment, mapping, …) follows as carets on it, the type-characteristics
;; extensions are `Characteristics:`, and the rest of the definition is caret'd at the top
(define type-characteristics-url "http://hl7.org/fhir/StructureDefinition/structuredefinition-type-characteristics")
(define (decompile-logical-resource js)
  (define logical? (equal? (hash-ref js 'kind #f) "logical"))
  (define name (entity-name js "Model"))
  (define parent (let ([p (hash-ref js 'baseDefinition #f)]) (and p (parent-name p))))
  (define exts (let ([x (hash-ref js 'extension '())]) (if (list? x) x '())))
  ;; each characteristic once: the compiler adds a local Parent's again (SUSHI's duplicate)
  (define characteristics
    (remove-duplicates
     (for/list ([e (in-list exts)] #:when (equal? (hash-ref e 'url #f) type-characteristics-url))
       (string-append "#" (hash-ref e 'valueCode "")))))
  (define other-exts (filter (lambda (e) (not (equal? (hash-ref e 'url #f) type-characteristics-url))) exts))
  (define meta (append (if parent (list (cons 'parent parent)) '()) (id-meta js) (sd-meta js)
                       (if (pair? characteristics) (list (cons 'characteristics characteristics)) '())))
  (define elts (hash-ref* js 'differential 'element))
  ;; skip the root element (id == the model id); the rest become addElement rules
  (define rules
    (append*
     (for/list ([elt (in-list elts)] #:unless (equal? (hash-ref elt 'path) (hash-ref js 'id)))
       (define rel (relative-path (hash-ref elt 'path)))
       (cons (rule-add-element rel
                               (card-str (hash-ref elt 'min 0) (hash-ref elt 'max "*"))
                               '()
                               ;; a logical model's type is its canonical: the name in FSH
                               (for/list ([t (in-list (hash-ref elt 'type '()))])
                                 (let ([c (hash-ref t 'code)]) (if (regexp-match? #rx"^https?://" c) (last-seg c) c)))
                               (hash-ref elt 'short #f) (hash-ref elt 'definition #f))
             (for/list ([prop (in-list (caret-rest elt '(path id min max base type short definition)))])
               (caret-on rel prop))))))
  ;; the other extensions keep their places after the characteristics AS PUBLISHED (the compiler
  ;; writes a local Parent's again, so their count is the published one, not the deduplicated)
  (define published-tc (count (lambda (e) (equal? (hash-ref e 'url #f) type-characteristics-url)) exts))
  (define ext-carets
    (append* (for/list ([e (in-list other-exts)] [i (in-naturals published-tc)])
               (value->carets (format "^extension[~a]" i) e))))
  (define top-carets (caret-rest js (append publisher-only '(resourceType id title description name url version baseDefinition type kind
                                                              derivation differential abstract fhirVersion extension))))
  (define all (append rules ext-carets top-carets))
  (if logical? (fsh-logical name meta all) (fsh-resource name meta all)))
;; parent-name : canonical -> string   the Parent's name without its version pin (a published IG
;; pins `Patient|4.0.1`, `Document|2.0.1`; GoFSH writes the name, and the forward compiler reads
;; a pinned core type too)
(define (parent-name p) (car (string-split (last-seg p) "|")))

;; Extension -> minimal FSH: emit only the user-meaningful rules and let the forward compiler
;; regenerate SUSHI's scaffold (root short/definition, extension 0..0 / min, per-slice .extension /
;; .url, url fixedUri). Rule order mirrors the forward fold so the round-trip element order matches.
;; an Extension through the profile machinery, minus the scaffold the forward compiler regenerates
;; (the root's short/definition from Title/Description, `extension` 0..0 or sliced by url,
;; `url` fixedUri, a sub-extension's own `url`/`extension`) — what stays: `contains` for the
;; sub-extensions, every value[x]'s type/binding/pattern/card, a sub-extension's short/definition,
;; and the definition's own context, status and abstract as carets
(define (extension-scaffold? js e)
  (define id (hash-ref e 'id ""))
  (cond [(equal? id "Extension")
         (and (equal? (hash-ref e 'short #f) (hash-ref js 'title (hash-ref js 'name #f)))
              (equal? (hash-ref e 'definition #f) (hash-ref js 'description (hash-ref js 'title (hash-ref js 'name #f))))
              (<= (hash-count (hash-remove (hash-remove (hash-remove (hash-remove e 'id) 'path) 'short) 'definition)) 0))]
        [(member id '("Extension.url" "Extension.extension")) #t]
        [(regexp-match? #px"^Extension\\.extension:[^.]+\\.(url|extension)$" id) #t]
        [else #f]))
(define (decompile-extension js)
  (define rules (sd-element-rules js "Extension" #:skip? (lambda (e) (extension-scaffold? js e))))
  (define top-carets (append (if (eq? (hash-ref js 'abstract #f) #t) (list (rule-caret #f "^abstract" #t)) '())
                             (caret-rest js (append publisher-only '(resourceType id title description name url version
                                                                     baseDefinition type kind derivation differential abstract fhirVersion mapping)))))
  (fsh-extension (entity-name js "Extension")
                 (append (id-meta js) (sd-meta js)) (append rules top-carets)))

(define (decompile-profile-extension js)
  (define base-type (hash-ref js 'type (hash-ref js 'id "Base")))
  (define name (entity-name js "Profile"))
  (define meta
    (append (let ([p (hash-ref js 'baseDefinition #f)]) (if p (list (cons 'parent (parent-name p))) '()))
            (id-meta js)
            (sd-meta js)))
  (define elem-rules (sd-element-rules js base-type))
  ;; `status` is NOT excluded: the forward compiler defaults it, so an overriding value must be
  ;; caret'd; `abstract` only when true (the compiler writes false)
  (define top-carets (append (if (eq? (hash-ref js 'abstract #f) #t) (list (rule-caret #f "^abstract" #t)) '())
                             (caret-rest js (append publisher-only '(resourceType id title description name url version
                                                                     baseDefinition type kind derivation differential abstract fhirVersion mapping)))))
  (fsh-profile name meta (append elem-rules top-carets)))

;; sd-element-rules : StructureDefinition base-type [#:skip? pred] -> rules   the differential as
;; FSH rules, slice-aware: a sliced base emits its ^slicing carets then its `contains`; a declared
;; slice (a type or a card of its own) is folded into that `contains`, a RESTATED slice (an
;; inherited one the profile only flags or annotates) is written at `path[slice]`; a RESLICED
;; slice (a slice that is itself a base) emits both; a sliced base the differential does not
;; carry (its slicing inherited) still gets its `contains`. Everything else emits ordinary rules.
(define (sd-element-rules js base-type #:skip? [skip? (lambda (e) #f)])
  (define elements0 (hash-ref* js 'differential 'element))
  ;; SUSHI's derived elements are not written: a slice child copied into every slice of the sliced
  ;; element (`section:X.extension:note` repeating `section.extension:note` — the compiler copies
  ;; them again) and a type slice's `[x]` parent slicing (made by the choice path)
  (define by-id (for/hash ([e (in-list elements0)] #:when (hash-ref e 'id #f)) (values (hash-ref e 'id) e)))
  (define (sans e) (hash-remove (hash-remove e 'id) 'path))
  (define (derived-copy? e)
    (define id (hash-ref e 'id ""))
    (define m (regexp-match #px"^([^:]+):[^.]+([.].*:.*)$" id))
    (and m (let ([u (hash-ref by-id (string-append (cadr m) (caddr m)) #f)]) (and u (equal? (sans u) (sans e))))))
  (define (type-slice? e)
    (define id (hash-ref e 'id ""))
    (define m (regexp-match #px"^(.*[.]|)([A-Za-z]+)\\[x\\]:([A-Za-z]+)$" id))
    (and m (hash-ref e 'sliceName #f) (string-prefix? (cadddr m) (caddr m))))
  (define type-sliced-ids
    (remove-duplicates (for/list ([e (in-list elements0)] #:when (type-slice? e)) (regexp-replace #px":[^.:]+$" (hash-ref e 'id) ""))))
  (define elements
    (for/list ([e (in-list elements0)] #:unless (derived-copy? e) #:unless (skip? e))
      (if (member (hash-ref e 'id #f) type-sliced-ids) (hash-remove e 'slicing) e)))
  ;; the root element's rules are written at `.` (a bare caret would be the definition's own); a
  ;; type slice (`performed[x]:performedDateTime`) is the choice path `performedDateTime`
  (define (rel-of elt)
    (define id (hash-ref elt 'id (hash-ref elt 'path)))
    (define r (id->fsh-rel base-type (if (type-slice? elt) (regexp-replace #px"[A-Za-z]+\\[x\\]:([A-Za-z]+)$" id "\\1") id)))
    (if (equal? r "") "." r))
  ;; slices: elements with a sliceName define a slice; group them under their base element id
  (define (declared-slice? e) (or (hash-ref e 'type #f) (hash-has-key? e 'min) (hash-has-key? e 'max)))
  (define slice-defs (filter (lambda (e) (and (hash-ref e 'sliceName #f) (not (type-slice? e)) (declared-slice? e))) elements))
  ;; the base element a slice belongs to: a reslice (id …:Lab/Chem) bases on …:Lab; else drop :slice
  (define (base-id-of e)
    (define id (hash-ref e 'id))
    (if (regexp-match? #px"/[^./]+$" id)
        (regexp-replace #px"/[^./]+$" id "")
        (regexp-replace #px":[^.:]+$" id "")))
  (define sliced-base-ids (remove-duplicates (map base-id-of slice-defs)))
  ;; an extension slice typed with an Extension (`type: Extension, profile: [url]`) is
  ;; `contains <url> named <slice>` — the compiler types it from the url, no `only` needed
  (define (extension-profile-of e)
    (define ts (hash-ref e 'type '()))
    (and (pair? ts) (equal? (hash-ref (car ts) 'code #f) "Extension")
         (let ([p (hash-ref (car ts) 'profile '())]) (and (pair? p) (unpin (car p))))))
  (define (contains-for bid)
    (rule-contains (id->fsh-rel base-type bid)
                   (for/list ([e (in-list slice-defs)] #:when (equal? (base-id-of e) bid))
                     ;; the local slice name is the last "/"-segment of sliceName (Chem in Lab/Chem)
                     (define sn (last (string-split (hash-ref e 'sliceName) "/")))
                     (define url (extension-profile-of e))
                     (contains-item (or url sn) (and url sn)
                                    (card-str (hash-ref e 'min 0) (hash-ref e 'max "*")) (elt-flags e)))))
  (define present-ids (for/list ([e (in-list elements)]) (hash-ref e 'id #f)))
  (define elem-rules
    (append*
     (for/list ([elt (in-list elements)])
       (define id (hash-ref elt 'id #f))
       (define base? (member id sliced-base-ids))
       (cond
         [(and (hash-ref elt 'sliceName #f) base? (declared-slice? elt))
          (append (element->rules elt (rel-of elt) #t) (list (contains-for id)))]
         ;; a type slice is written at its choice path with its own card (the compiler makes the
         ;; `[x]` element's type slicing from it)
         [(type-slice? elt) (element->rules elt (rel-of elt) #f)]
         ;; a plain slice: its card and flags ride in the `contains`; what else it says (a pattern,
         ;; a binding, a short, its constraints) is written at `path[slice]`
         [(and (hash-ref elt 'sliceName #f) (declared-slice? elt))
          (filter (lambda (r) (not (or (rule-flag? r) (and (extension-profile-of elt) (rule-only? r)))))
                  (element->rules elt (rel-of elt) #t))]
         ;; a restated slice: flags and annotations at `path[slice]`
         [(hash-ref elt 'sliceName #f) (element->rules elt (rel-of elt) #t)]
         [base? (append (element->rules elt (rel-of elt) #t) (list (contains-for id)))]
         [else (element->rules elt (rel-of elt) #f)]))))
  ;; a sliced base the differential does not carry (`bodySite.extension` under a dependency's
  ;; profile): its `contains` alone
  (define absent-bases
    (for/list ([bid (in-list sliced-base-ids)] #:unless (member bid present-ids)) (contains-for bid)))
  (append absent-bases elem-rules))

;; card-str : min max -> "min..max"   a published package's integers may read back inexact (1.0)
(define (card-str mn mx)
  (define (int x) (if (and (number? x) (integer? x)) (inexact->exact x) x))
  (format "~a..~a" (int mn) (int mx)))
;; the FSH entity name is the resource `name` (falls back to id); Id: is emitted separately
(define (entity-name js default) (str (hash-ref js 'name (hash-ref js 'id default))))
(define (id-meta js) (let ([i (hash-ref js 'id #f)]) (if i (list (cons 'id i)) '())))

;; an ElementDefinition id -> its FSH relative path: drop the "Type." prefix, ":slice" -> "[slice]"
(define (id->fsh-rel base-type id)
  (cond
    [(equal? id base-type) ""]
    [(string-prefix? id (string-append base-type "."))
     (regexp-replace* #px":([^.\\[]+)" (substring id (add1 (string-length base-type))) "[\\1]")]
    [else (regexp-replace* #px":([^.\\[]+)" id "[\\1]")]))

;; a differential element -> idiomatic rules at `rel` (skip cardinality when it is derived, e.g. a
;; sliced base whose min is the sum of its slice minimums)
(define (element->rules elt rel skip-card?)
  (define out '())
  (define (add! r) (set! out (cons r out)))
  (let ([flags (elt-flags elt)])
    (cond
      [skip-card? (when (pair? flags) (add! (rule-flag rel '() flags)))]
      ;; only what the element states: `1..` when it has no max (a round trip must not invent "*")
      [(or (hash-has-key? elt 'min) (hash-has-key? elt 'max))
       (add! (rule-card rel (card-str (hash-ref elt 'min "") (hash-ref elt 'max "")) flags))]
      [(pair? flags) (add! (rule-flag rel '() flags))]))
  ;; an explicit `mustSupport: false` is not a flag; it is said as a caret
  (when (eq? (hash-ref elt 'mustSupport 'absent) #f) (add! (rule-caret rel "^mustSupport" #f)))
  (let ([b (hash-ref elt 'binding #f)])
    (when (and b (hash-ref b 'valueSet #f))
      (add! (rule-binding rel (hash-ref b 'valueSet) (string->symbol (hash-ref b 'strength "required")))))
    ;; the binding's description and extensions (additional bindings) follow as carets
    (when (hash? b)
      (for ([prop (in-list (value->carets "^binding" (hash-remove (hash-remove b 'valueSet) 'strength)))])
        (add! (caret-on rel prop)))))
  (let ([ts (hash-ref elt 'type #f)])
    (when ts (add! (rule-only rel (map type->fsh-str ts)))))
  (let ([cs (hash-ref elt 'constraint #f)])
    (when cs (add! (rule-obeys rel (for/list ([c (in-list cs)]) (hash-ref c 'key))))))
  ;; fixed[x] / pattern[x] / slicing / short / definition / any other element property -> caret rule
  ;; on this element
  (for ([prop (in-list (caret-rest elt '(path id min max base mustSupport isSummary isModifier
                                         binding type constraint sliceName mapping)))])
    (add! (caret-on rel prop)))
  (reverse out))

(define (elt-flags elt)
  (append (if (hash-ref elt 'mustSupport #f) '(MS) '())
          (if (hash-ref elt 'isSummary #f) '(SU) '())
          (if (hash-ref elt 'isModifier #f) (list (string->symbol "?!")) '())))

(define (caret-on rel r) (match r [(rule-caret #f cp v) (rule-caret rel cp v)] [_ r]))

;; what the IG Publisher writes into a published resource and no FSH says: the snapshot, the
;; narrative, and the dates/publisher/contact/jurisdiction from the IG's configuration
(define publisher-only '(snapshot text date publisher contact jurisdiction))
;; ---- caret catch-all -----------------------------------------------------------------------
(define (caret-rest js handled)
  (append*
   (for/list ([k (in-list (sort (hash-keys js) symbol<?))] #:unless (memq k handled))
     (value->carets (format "^~a" (symbol->string k)) (hash-ref js k)))))

(define (value->carets caret js)
  (cond
    [(hash? js) (append* (for/list ([k (in-list (sort (hash-keys js) symbol<?))])
                           (value->carets (format "~a.~a" caret (symbol->string k)) (hash-ref js k))))]
    [(list? js) (append* (for/list ([elt (in-list js)] [i (in-naturals)])
                           (value->carets (format "~a[~a]" caret i) elt)))]
    [else (list (rule-caret #f caret js))]))

;; ---- shared helpers ------------------------------------------------------------------------
(define (sd-meta js)
  (append (let ([t (hash-ref js 'title #f)]) (if t (list (cons 'title t)) '()))
          (let ([d (hash-ref js 'description #f)]) (if d (list (cons 'description d)) '()))))
;; strip the leading "Type." — a single-segment path IS the root element, so its relative path is ""
(define (relative-path path)
  (define segs (string-split path "."))
  (if (> (length segs) 1) (string-join (cdr segs) ".") ""))

;; an ElementDefinition.type entry -> its FSH `only`-target string: Reference/Canonical targets kept,
;; a profile of the type named by its canonical (`only <url>`; the compiler takes its base type as the
;; code), version pins dropped
(define (type->fsh-str t)
  (define code (hash-ref t 'code))
  (define tps (hash-ref t 'targetProfile '()))
  (define pfs (let ([p (hash-ref t 'profile '())]) (if (list? p) p '())))
  (cond [(pair? tps)
         (format "~a(~a)" (case code [("canonical") "Canonical"] [else code])
                 (string-join (map (lambda (u) (unpin (last-seg u))) tps) " or "))]
        [(pair? pfs) (string-join (map unpin pfs) " or ")]
        [else code]))
(define (unpin s) (car (string-split s "|")))
(define (last-seg s) (last (string-split s "/")))
(define (hash-ref* js k1 k2)
  (let ([a (hash-ref js k1 #f)]) (if (hash? a) (hash-ref a k2 '()) '())))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit)
  ;; an instance: one assignment per primitive leaf, arrays indexed, the type and id as metadata
  (define pat (hasheq 'resourceType "Patient" 'id "p1" 'gender "female" 'active #t
                      'name (list (hasheq 'family "Doe" 'given '("Jane" "J")))))
  (define e (fhir->fsh-entity pat))
  (check-true (fsh-instance? e))
  (check-equal? (fsh-instance-name e) "p1")
  (check-equal? (fsh-instance-metadata e) (list (cons 'instanceOf "Patient") (cons 'usage "#example")))
  (check-equal? (fsh-instance-rules e)
                (list (rule-assign "active" #t #f) (rule-assign "gender" "female" #f)
                      (rule-assign "name[0].family" "Doe" #f)
                      (rule-assign "name[0].given[0]" "Jane" #f) (rule-assign "name[0].given[1]" "J" #f)))
  (check-equal? (fhir->fsh pat)
                "Instance: p1\nInstanceOf: Patient\nUsage: #example\n* active = true\n* gender = \"female\"\n* name[0].family = \"Doe\"\n* name[0].given[0] = \"Jane\"\n* name[0].given[1] = \"J\"")
  ;; a value set: its enumerated concepts as include rules, a whole system as `codes from system`
  (define vs (hasheq 'resourceType "ValueSet" 'id "vs1" 'name "VS1" 'url "http://x/ValueSet/vs1" 'status "active"
                     'compose (hasheq 'include (list (hasheq 'system "http://x/cs" 'concept (list (hasheq 'code "a" 'display "A")))
                                                     (hasheq 'system "http://y/cs")))))
  (define v (fhir->fsh-entity vs))
  (check-true (fsh-valueset? v))
  (check-equal? (fsh-valueset-name v) "VS1")
  (check-equal? (length (filter vs-component? (fsh-valueset-rules v))) 2)
  (check-true (regexp-match? #rx"\\* include http://x/cs#a \"A\"" (fhir->fsh vs)))
  (check-true (regexp-match? #rx"\\* include codes from system http://y/cs" (fhir->fsh vs)))
  ;; a code system: the concept tree as `* #a #b` rules
  (define cs (hasheq 'resourceType "CodeSystem" 'id "cs1" 'name "CS1" 'url "http://x/cs" 'status "active" 'content "complete"
                     'concept (list (hasheq 'code "a" 'display "A" 'concept (list (hasheq 'code "b"))))))
  (check-equal? (filter concept-rule? (fsh-codesystem-rules (fhir->fsh-entity cs)))
                (list (concept-rule '("#a") "A" #f) (concept-rule '("#a" "#b") #f #f)))
  ;; a profile's constraints come back as Invariant entities too
  (define sd (hasheq 'resourceType "StructureDefinition" 'id "bp" 'name "BP" 'url "http://x/StructureDefinition/bp"
                     'type "Observation" 'baseDefinition "http://hl7.org/fhir/StructureDefinition/Observation"
                     'derivation "constraint" 'status "active" 'kind "resource" 'abstract #f
                     'differential (hasheq 'element (list (hasheq 'id "Observation.component" 'path "Observation.component" 'min 2
                                                                  'constraint (list (hasheq 'key "bp-1" 'severity "error" 'human "two" 'expression "component.count() = 2")))))))
  (define doc (fhir->fsh sd))
  (check-true (regexp-match? #rx"Profile: BP" doc))
  (check-true (regexp-match? #rx"Invariant: bp-1" doc))
  (check-true (regexp-match? #rx"Severity: #error" doc)))
