#lang racket/base
;;; terminology.rkt — ValueSet / CodeSystem -> compose / concept.
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
         "caret-apply.rkt")
(provide
 (contract-out
  [compile-valueset (-> fsh-valueset? hash? hash?)]
  [compile-codesystem (-> fsh-codesystem? hash? hash?)]))
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
      ;; FHIR's FilterOperator code is spelled `descendent-of`; FSH accepts `descendant-of` for it
      (drop-false (hasheq 'property prop 'op (if (equal? op "descendant-of") "descendent-of" op)
                          'value (and (pair? val) (filter-value (car val)))))))
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
