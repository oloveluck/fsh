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
         (only-in "print.rkt" print-doc print-entity)
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
                           (opt-meta c 'expression 'expression values))
                   '())))

(define (opt-meta h k mk f) (let ([v (hash-ref h k #f)]) (if v (list (cons mk (f v))) '())))

(define (fhir->fsh-entity js)
  (case (and (hash? js) (hash-ref js 'resourceType #f))
    [("StructureDefinition") (decompile-sd js)]
    [("ValueSet")            (decompile-valueset js)]
    [("CodeSystem")          (decompile-codesystem js)]
    [else                    (decompile-instance js)]))

;; ---- Instance — per-leaf assignment walk (lossless) ----------------------------------------
(define (decompile-instance js)
  (define type (hash-ref js 'resourceType))
  (define id (hash-ref js 'id "Example"))
  (fsh-instance (str id) (list (cons 'instanceOf type) (cons 'usage "#example")) (walk js "" #t)))

(define (walk js path top?)
  (cond
    [(hash? js)
     (append*
      (for/list ([k (in-list (sort (hash-keys js) symbol<?))]
                 #:unless (and top? (memq k '(resourceType id)))
                 #:unless (underscore? k))
        (define here (extend path (symbol->string k)))
        (append (walk (hash-ref js k) here #f)
                (let ([sib (hash-ref js (underscore k) #f)]) (if sib (walk sib here #f) '())))))]
    [(list? js)
     (append* (for/list ([elt (in-list js)] [i (in-naturals)]) (walk elt (format "~a[~a]" path i) #f)))]
    [else (list (rule-assign path js #f))]))

(define (extend path seg) (if (= 0 (string-length path)) seg (string-append path "." seg)))
(define (underscore? k) (char=? (string-ref (symbol->string k) 0) #\_))
(define (underscore k) (string->symbol (string-append "_" (symbol->string k))))
(define (str x) (if (string? x) x (format "~a" x)))

;; ---- ValueSet / CodeSystem -----------------------------------------------------------------
(define (decompile-valueset js)
  (define rules
    (append
     (append-map (lambda (inc) (vs-include->rules inc #t)) (hash-ref* js 'compose 'include))
     (append-map (lambda (exc) (vs-include->rules exc #f)) (hash-ref* js 'compose 'exclude))
     (caret-rest js '(resourceType id title description compose name url status version))))
  (fsh-valueset (entity-name js "ValueSet") (append (id-meta js) (sd-meta js)) rules))

;; an include/exclude component -> one FSH vs-component per concept (or a whole-system `codes from`)
(define (vs-include->rules inc include?)
  (define sys (hash-ref inc 'system #f))
  (define concepts (hash-ref inc 'concept '()))
  (cond
    [(null? concepts)
     (if sys
         (list (vs-component include? (list (list 'vsFilterComponent "codes"
                                                  (list 'vsComponentFrom "from" (list 'vsFromSystem "system" (list 'name sys)))))))
         '())]
    [else
     (for/list ([c (in-list concepts)])
       (define disp (hash-ref c 'display #f))
       (define code-node (if disp
                             (list 'code (code-tok (format "~a#~a" (or sys "") (hash-ref c 'code))) disp)
                             (list 'code (code-tok (format "~a#~a" (or sys "") (hash-ref c 'code))))))
       (vs-component include? (list (list 'vsConceptComponent code-node))))]))

(define (decompile-codesystem js)
  (define rules
    (append
     (concepts->rules (hash-ref js 'concept '()) '())
     (caret-rest js '(resourceType id title description concept name content url status version count))))
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

;; Logical / custom Resource -> addElement rules (element paths root at the entity id)
(define (decompile-logical-resource js)
  (define logical? (equal? (hash-ref js 'kind #f) "logical"))
  (define name (entity-name js "Model"))
  (define parent (let ([p (hash-ref js 'baseDefinition #f)]) (and p (last-seg p))))
  (define meta (append (if parent (list (cons 'parent parent)) '()) (id-meta js) (sd-meta js)))
  (define elts (hash-ref* js 'differential 'element))
  ;; skip the root element (id == the model id); the rest become addElement rules
  (define rules
    (for/list ([elt (in-list elts)] #:unless (equal? (hash-ref elt 'path) (hash-ref js 'id)))
      (rule-add-element (relative-path (hash-ref elt 'path))
                        (format "~a..~a" (hash-ref elt 'min 0) (hash-ref elt 'max "*"))
                        '()
                        (for/list ([t (in-list (hash-ref elt 'type '()))]) (hash-ref t 'code))
                        (hash-ref elt 'short #f) (hash-ref elt 'definition #f))))
  (if logical? (fsh-logical name meta rules) (fsh-resource name meta rules)))

;; Extension -> minimal FSH: emit only the user-meaningful rules and let the forward compiler
;; regenerate SUSHI's scaffold (root short/definition, extension 0..0 / min, per-slice .extension /
;; .url, url fixedUri). Rule order mirrors the forward fold so the round-trip element order matches.
(define (decompile-extension js)
  (define elts (hash-ref* js 'differential 'element))
  (define (by-id id) (findf (lambda (e) (equal? (hash-ref e 'id #f) id)) elts))
  (define (types-of e) (for/list ([t (in-list (hash-ref e 'type '()))]) (hash-ref t 'code)))
  ;; sub-extension slices (id "Extension.extension:NAME", sliceName present)
  (define slices
    (for/list ([e (in-list elts)]
               #:when (regexp-match? #px"^Extension\\.extension:[^.]+$" (hash-ref e 'id "")))
      (hash-ref e 'sliceName)))
  (define contains-rule
    (and (pair? slices)
         (rule-contains "extension"
                        (for/list ([sn (in-list slices)])
                          (define se (by-id (string-append "Extension.extension:" sn)))
                          (contains-item sn #f (format "~a..~a" (hash-ref se 'min 0) (hash-ref se 'max "*")) '())))))
  (define slice-value-rules
    (for/list ([sn (in-list slices)]
               #:when (let ([ve (by-id (format "Extension.extension:~a.value[x]" sn))]) (and ve (pair? (types-of ve)))))
      (rule-only (format "extension[~a].value[x]" sn)
                 (types-of (by-id (format "Extension.extension:~a.value[x]" sn))))))
  (define value-el (by-id "Extension.value[x]"))
  (define value-rule
    (cond [(and value-el (pair? (types-of value-el))) (rule-only "value[x]" (types-of value-el))]
          [(and value-el (equal? (hash-ref value-el 'max #f) "0")) (rule-card "value[x]" "0..0" '())]
          [else #f]))
  (define rules (filter values (append (list contains-rule) slice-value-rules (list value-rule))))
  (fsh-extension (entity-name js "Extension")
                 (append (id-meta js) (sd-meta js)) rules))

(define (decompile-profile-extension js)
  (define base-type (hash-ref js 'type (hash-ref js 'id "Base")))
  (define name (entity-name js "Profile"))
  (define meta
    (append (let ([p (hash-ref js 'baseDefinition #f)]) (if p (list (cons 'parent (last-seg p))) '()))
            (id-meta js)
            (sd-meta js)))
  (define elements (hash-ref* js 'differential 'element))
  (define (rel-of elt) (id->fsh-rel base-type (hash-ref elt 'id (hash-ref elt 'path))))
  ;; slices: elements with a sliceName define a slice; group them under their base element id
  (define slice-defs (filter (lambda (e) (hash-ref e 'sliceName #f)) elements))
  ;; the base element a slice belongs to: a reslice (id …:Lab/Chem) bases on …:Lab; else drop :slice
  (define (base-id-of e)
    (define id (hash-ref e 'id))
    (if (regexp-match? #px"/[^./]+$" id)
        (regexp-replace #px"/[^./]+$" id "")
        (regexp-replace #px":[^.:]+$" id "")))
  (define sliced-base-ids (remove-duplicates (map base-id-of slice-defs)))
  (define (contains-for bid)
    (rule-contains (id->fsh-rel base-type bid)
                   (for/list ([e (in-list slice-defs)] #:when (equal? (base-id-of e) bid))
                     ;; the local slice name is the last "/"-segment of sliceName (Chem in Lab/Chem)
                     (contains-item (last (string-split (hash-ref e 'sliceName) "/")) #f
                                    (format "~a..~a" (hash-ref e 'min 0) (hash-ref e 'max "*")) (elt-flags e)))))
  ;; per-element rules, slice-aware: a sliced base emits its ^slicing carets then its `contains`; a
  ;; plain slice is folded into its parent `contains`; a RESLICED slice (a slice that is itself a base)
  ;; emits both. Everything else emits ordinary rules.
  (define elem-rules
    (append*
     (for/list ([elt (in-list elements)])
       (define id (hash-ref elt 'id #f))
       (define base? (member id sliced-base-ids))
       (cond
         [(and (hash-ref elt 'sliceName #f) base?) (append (element->rules elt (rel-of elt) #t) (list (contains-for id)))]
         [(hash-ref elt 'sliceName #f) '()]
         [base? (append (element->rules elt (rel-of elt) #t) (list (contains-for id)))]
         [else (element->rules elt (rel-of elt) #f)]))))
  ;; `status` is NOT excluded: the forward compiler defaults it, so an overriding value must be caret'd
  (define top-carets (caret-rest js '(resourceType id title description name url version
                                       baseDefinition type kind derivation differential abstract fhirVersion mapping)))
  (fsh-profile name meta (append elem-rules top-carets)))

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
      [(or (hash-has-key? elt 'min) (hash-has-key? elt 'max))
       (add! (rule-card rel (format "~a..~a" (hash-ref elt 'min 0) (hash-ref elt 'max "*")) flags))]
      [(pair? flags) (add! (rule-flag rel '() flags))]))
  (let ([b (hash-ref elt 'binding #f)])
    (when (and b (hash-ref b 'valueSet #f))
      (add! (rule-binding rel (hash-ref b 'valueSet) (string->symbol (hash-ref b 'strength "required"))))))
  (let ([ts (hash-ref elt 'type #f)])
    (when ts (add! (rule-only rel (map type->fsh-str ts)))))
  (let ([cs (hash-ref elt 'constraint #f)])
    (when cs (add! (rule-obeys rel (for/list ([c (in-list cs)]) (hash-ref c 'key))))))
  ;; fixed[x] / pattern[x] / slicing / any other element property -> caret rule on this element
  (for ([prop (in-list (caret-rest elt '(path id min max base mustSupport isSummary isModifier
                                         binding type short definition constraint sliceName mapping)))])
    (add! (caret-on rel prop)))
  (reverse out))

(define (elt-flags elt)
  (append (if (hash-ref elt 'mustSupport #f) '(MS) '())
          (if (hash-ref elt 'isSummary #f) '(SU) '())
          (if (hash-ref elt 'isModifier #f) (list (string->symbol "?!")) '())))

(define (caret-on rel r) (match r [(rule-caret #f cp v) (rule-caret rel cp v)] [_ r]))

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

;; an ElementDefinition.type entry -> its FSH `only`-target string, preserving Reference/Canonical targets
(define (type->fsh-str t)
  (define code (hash-ref t 'code))
  (define tps (hash-ref t 'targetProfile '()))
  (if (pair? tps)
      (format "~a(~a)" (case code [("canonical") "Canonical"] [else code])
              (string-join (map last-seg tps) " or "))
      code))
(define (last-seg s) (last (string-split s "/")))
(define (hash-ref* js k1 k2)
  (let ([a (hash-ref js k1 #f)]) (if (hash? a) (hash-ref a k2 '()) '())))
