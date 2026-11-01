#lang racket/base

;;; ast.rkt — the FSH AST and lowering from brag's parse-to-datum.
;;;
;;; Structs are #:prefab so entity ASTs cross the #lang reader -> expander boundary as
;;; literal data. `lower-entity` turns one entity's parse-datum into an entity struct;
;;; metadata becomes an assoc list, rules become rule structs. FSH path strings are kept raw
;;; (parsed on demand by path.rkt). `code-tok` tags CODE tokens so concept/codeCaret can
;;; tell codes from display strings in the flattened datum.

(require racket/match
         racket/contract)

(provide (struct-out code-tok)
         ;; entities
         (struct-out fsh-alias) (struct-out fsh-profile) (struct-out fsh-extension)
         (struct-out fsh-logical) (struct-out fsh-resource) (struct-out fsh-instance)
         (struct-out fsh-invariant) (struct-out fsh-valueset) (struct-out fsh-codesystem)
         (struct-out fsh-ruleset) (struct-out fsh-param-ruleset) (struct-out fsh-mapping)
         ;; rules
         (struct-out rule-card) (struct-out rule-flag) (struct-out rule-binding)
         (struct-out rule-assign) (struct-out rule-contains) (struct-out rule-only)
         (struct-out rule-obeys) (struct-out rule-caret) (struct-out rule-code-caret)
         (struct-out rule-mapping) (struct-out rule-insert) (struct-out rule-code-insert)
         (struct-out rule-add-element) (struct-out rule-add-cr-element) (struct-out rule-path)
         (struct-out vs-component) (struct-out concept-rule) (struct-out contains-item)
         ;; values
         (struct-out fsh-code) (struct-out fsh-quantity) (struct-out fsh-ratio)
         (struct-out fsh-reference) (struct-out fsh-canonical)
         (contract-out
          [lower-entity (-> any/c any/c)]))   ; brag parse-tree datum -> an FSH entity AST

;; CODE token tag (set by the lexer)
(struct code-tok (text) #:prefab)

;; entities — metadata is (listof (cons symbol any)); rules is (listof rule-struct)
(struct fsh-alias     (name value) #:prefab)
(struct fsh-profile   (name metadata rules) #:prefab)
(struct fsh-extension (name metadata rules) #:prefab)
(struct fsh-logical   (name metadata rules) #:prefab)
(struct fsh-resource  (name metadata rules) #:prefab)
(struct fsh-instance  (name metadata rules) #:prefab)
(struct fsh-invariant (name metadata rules) #:prefab)
(struct fsh-valueset  (name metadata rules) #:prefab)
(struct fsh-codesystem(name metadata rules) #:prefab)
(struct fsh-ruleset   (name rules) #:prefab)
;; parameterized RuleSet: body captured raw (list of (cons type value)) for insert-time expansion
(struct fsh-param-ruleset (name params body) #:prefab)
(struct fsh-mapping   (name metadata rules) #:prefab)

;; rules (path strings are raw; parse with path.rkt)
(struct rule-card        (path card flags) #:prefab)
(struct rule-flag        (path extra-paths flags) #:prefab)
(struct rule-binding     (path valueset strength) #:prefab)
(struct rule-assign      (path value exact?) #:prefab)
(struct rule-contains    (path items) #:prefab)
(struct rule-only        (path types) #:prefab)
(struct rule-obeys       (path invariants) #:prefab)
(struct rule-caret       (path caret-path value) #:prefab)
(struct rule-code-caret  (codes caret-path value) #:prefab)
(struct rule-mapping     (path target comment code) #:prefab)
(struct rule-insert      (path ref) #:prefab)
(struct rule-code-insert (codes ref) #:prefab)
(struct rule-add-element (path card flags types short definition) #:prefab)
(struct rule-add-cr-element (path card flags ref short definition) #:prefab)
(struct rule-path        (path) #:prefab)
(struct vs-component     (include? body) #:prefab)
(struct concept-rule     (codes display definition) #:prefab)
(struct contains-item    (name named-as card flags) #:prefab)

;; values
(struct fsh-code      (code display) #:prefab)
(struct fsh-quantity  (value unit display) #:prefab)
(struct fsh-ratio     (numerator denominator) #:prefab)
(struct fsh-reference (target display) #:prefab)
(struct fsh-canonical (target) #:prefab)

;; -------------------------------------------------------------------
;; lowering
;; -------------------------------------------------------------------
(define (lower-entity datum)
  (match datum
    [(list 'entity inner) (lower-entity inner)]

    [(list 'alias (list 'name n) v)        (fsh-alias n (code-text v))]
    [(list-rest 'profile (list 'name n) rest)   (apply fsh-profile n (split rest))]
    [(list-rest 'extension (list 'name n) rest) (apply fsh-extension n (split rest))]
    [(list-rest 'logical (list 'name n) rest)   (apply fsh-logical n (split rest))]
    [(list-rest 'resource (list 'name n) rest)  (apply fsh-resource n (split rest))]
    [(list-rest 'instance (list 'name n) rest)  (apply fsh-instance n (split rest))]
    [(list-rest 'invariant (list 'name n) rest) (apply fsh-invariant n (split rest))]
    [(list-rest 'valueSet (list 'name n) rest)  (apply fsh-valueset n (split rest))]
    [(list-rest 'codeSystem (list 'name n) rest)(apply fsh-codesystem n (split rest))]
    [(list-rest 'mapping (list 'name n) rest)   (apply fsh-mapping n (split rest))]
    [(list-rest 'ruleSet ref rest)
     (fsh-ruleset (if (string? ref) ref (code-text ref))
                  (map (lambda (w) (lower-rule (wrapper-inner w))) rest))]
    [_ (error 'lower-entity "unrecognized entity: ~s" datum)]))

;; partition entity children into (metadata-alist rules-list)
(define (split children)
  (let loop ([cs children] [meta '()] [rules '()])
    (cond
      [(null? cs) (list (reverse meta) (reverse rules))]
      [else
       (define inner (wrapper-inner (car cs)))
       (define head (and (pair? inner) (car inner)))
       (if (metadata-head? head)
           (loop (cdr cs) (cons (lower-meta inner) meta) rules)
           (loop (cdr cs) meta (cons (lower-rule inner) rules)))])))

(define (wrapper-inner w)
  ;; unwrap nested wrappers: (ruleSetRule (sdRule (cardRule …))) -> (cardRule …)
  (let loop ([w w])
    (match w
      [(list (? wrapper-tag?) inner) (loop inner)]
      [_ w])))

(define (wrapper-tag? s)
  (memq s '(sdMetadata instanceMetadata invariantMetadata vsMetadata csMetadata
            mappingMetadata sdRule lrRule instanceRule invariantRule vsRule csRule
            ruleSetRule mappingEntityRule)))

(define (metadata-head? h)
  (memq h '(parent id title description expression xpath severity instanceOf usage
            source target context characteristics)))

(define (lower-meta inner)
  (match inner
    [(list 'parent (list 'name n))     (cons 'parent n)]
    [(list 'id (list 'name n))         (cons 'id n)]
    [(list 'instanceOf (list 'name n)) (cons 'instanceOf n)]
    [(list 'source (list 'name n))     (cons 'source n)]
    [(list 'title s)                   (cons 'title s)]
    [(list 'description s)             (cons 'description s)]
    [(list 'expression s)             (cons 'expression s)]
    [(list 'xpath s)                  (cons 'xpath s)]
    [(list 'target s)                 (cons 'target s)]
    [(list 'usage c)                  (cons 'usage (code-text c))]
    [(list 'severity c)              (cons 'severity (code-text c))]
    [(list-rest 'context items)       (cons 'context items)]
    [(list-rest 'characteristics items) (cons 'characteristics (map code-text items))]
    [_ (cons 'unknown inner)]))

(define (lower-rule inner)
  (match inner
    [(list 'cardRule (list 'path p) card flags ...)
     (rule-card p card (map flag-sym flags))]
    [(list 'flagRule (list 'path p) rest ...)
     (define-values (paths flags) (split-paths/flags rest))
     (rule-flag p paths (map flag-sym flags))]
    [(list 'valueSetRule (list 'path p) _from (list 'name vs) maybe-strength ...)
     (rule-binding p vs (and (pair? maybe-strength) (strength-sym (car maybe-strength))))]
    [(list 'fixedValueRule (list 'path p) (list 'value v) rest ...)
     (rule-assign p (lower-value v) (and (member "(exactly)" rest) #t))]
    [(list 'containsRule (list 'path p) _contains items ...)
     (rule-contains p (lower-items items))]
    [(list 'onlyRule (list 'path p) _only types ...)
     (rule-only p (filter-map-targettypes types))]
    [(list 'obeysRule rest ...)
     (define-values (path invs) (lower-obeys rest))
     (rule-obeys path invs)]
    [(list 'caretValueRule rest ...)
     (lower-caret rest)]
    [(list 'codeCaretValueRule rest ...)
     (lower-code-caret rest)]
    [(list 'mappingRule rest ...) (lower-mapping rest)]
    [(list 'insertRule rest ...)  (lower-insert rest)]
    [(list 'codeInsertRule rest ...) (lower-code-insert rest)]
    [(list 'pathRule (list 'path p)) (rule-path p)]
    [(list 'addElementRule (list 'path p) card rest ...)
     (lower-add-element p card rest)]
    [(list 'addCRElementRule (list 'path p) card rest ...)
     (lower-add-cr p card rest)]
    [(list-rest 'vsComponent rest) (lower-vscomponent rest)]
    [(list-rest 'concept rest) (lower-concept rest)]
    [_ (error 'lower-rule "unrecognized rule: ~s" inner)]))

;; ---- rule helpers --------------------------------------------------
(define (flag-sym f) (match f [(list 'flag s) (string->symbol s)] [_ f]))
(define (strength-sym s)
  (match s [(list 'strength t) (string->symbol (regexp-replace* #px"[()]" t ""))] [_ s]))

(define (split-paths/flags rest)
  ;; rest = ("and" (path p))* (flag f)+   after the first path
  (let loop ([rest rest] [paths '()] [flags '()])
    (match rest
      ['() (values (reverse paths) (reverse flags))]
      [(list-rest "and" (list 'path p) more) (loop more (cons p paths) flags)]
      [(list-rest (and f (list 'flag _)) more) (loop more paths (cons f flags))]
      [(list-rest _ more) (loop more paths flags)])))

(define (filter-map-targettypes types)
  (for/list ([t (in-list types)] #:when (and (pair? t) (eq? (car t) 'targetType)))
    (lower-targettype (cadr t))))

(define (lower-targettype t)
  (match t
    [(list 'name n) n]
    [(list 'referenceType s) s]
    [(list 'canonical s) s]
    [(list 'codeableReferenceType s) s]
    [(? string? s) s]
    [_ t]))

(define (lower-items items)
  ;; items separated by "and"
  (for/list ([it (in-list items)] #:when (and (pair? it) (eq? (car it) 'item)))
    (match it
      [(list 'item (list 'name n) "named" (list 'name a) card flags ...)
       (contains-item n a card (map flag-sym flags))]
      [(list 'item (list 'name n) card flags ...)
       (contains-item n #f card (map flag-sym flags))])))

(define (lower-obeys rest)
  ;; rest = path? KW_OBEYS name (KW_AND name)*
  (let loop ([rest rest] [path #f] [invs '()])
    (match rest
      ['() (values path (reverse invs))]
      [(list-rest (list 'path p) more) (loop more p invs)]
      [(list-rest "obeys" more) (loop more path invs)]
      [(list-rest "and" more) (loop more path invs)]
      [(list-rest (list 'name n) more) (loop more path (cons n invs))]
      [(list-rest _ more) (loop more path invs)])))

(define (lower-caret rest)
  ;; rest = path? caretPath value
  (define path (match rest [(cons (list 'path p) _) p] [_ #f]))
  (define cp   (for/or ([x (in-list rest)]) (match x [(list 'caretPath c) c] [_ #f])))
  (define v    (for/or ([x (in-list rest)]) (match x [(list 'value vv) (lower-value vv)] [_ #f])))
  (rule-caret path cp v))

(define (lower-code-caret rest)
  (define codes (for/list ([x (in-list rest)] #:when (code-tok? x)) (code-tok-text x)))
  (define cp (for/or ([x (in-list rest)]) (match x [(list 'caretPath c) c] [_ #f])))
  (define v  (for/or ([x (in-list rest)]) (match x [(list 'value vv) (lower-value vv)] [_ #f])))
  (rule-code-caret codes cp v))

(define (lower-mapping rest)
  ;; path? ARROW STRING (STRING|MULTILINE)? CODE?
  (define path (match rest [(cons (list 'path p) _) p] [_ #f]))
  (define strs (filter string? rest))
  (define code (for/or ([x (in-list rest)]) (and (code-tok? x) (code-tok-text x))))
  (rule-mapping path (and (pair? strs) (car strs)) (and (pair? strs) (pair? (cdr strs)) (cadr strs)) code))

(define (lower-insert rest)
  (define path (match rest [(cons (list 'path p) _) p] [_ #f]))
  (rule-insert path (lower-ref rest)))
(define (lower-code-insert rest)
  (define codes (for/list ([x (in-list rest)] #:when (code-tok? x)) (code-tok-text x)))
  (rule-code-insert codes (lower-ref rest)))
(define (lower-ref rest)
  (for/or ([x (in-list rest)])
    (cond [(string? x) x]                                   ; RULESET_REFERENCE
          [(and (pair? x) (eq? (car x) 'paramRuleSetRef)) (cdr x)]
          [else #f])))

(define (lower-add-element p card rest)
  (define strs (filter string? (filter-non-target rest)))
  (rule-add-element p card (collect-flags rest) (filter-map-targettypes rest)
                    (and (pair? strs) (car strs))
                    (and (pair? strs) (pair? (cdr strs)) (cadr strs))))
(define (lower-add-cr p card rest)
  (define strs (filter string? (filter-non-target rest)))
  (rule-add-cr-element p card (collect-flags rest) #f
                       (and (pair? strs) (car strs))
                       (and (pair? strs) (pair? (cdr strs)) (cadr strs))))
(define (filter-non-target rest)
  (filter (lambda (x) (not (and (pair? x) (memq (car x) '(targetType flag))))) rest))
(define (collect-flags rest)
  (for/list ([x (in-list rest)] #:when (and (pair? x) (eq? (car x) 'flag))) (flag-sym x)))

(define (lower-vscomponent rest)
  (define include? (cond [(member "include" rest) #t] [(member "exclude" rest) #f] [else #t]))
  (vs-component include? (filter (lambda (x) (not (member x '("include" "exclude")))) rest)))

(define (lower-concept rest)
  ;; CODE+ STRING? STRING?
  (define codes (for/list ([x (in-list rest)] #:when (code-tok? x)) (code-tok-text x)))
  (define strs (filter string? rest))
  (concept-rule codes (and (pair? strs) (car strs))
                (and (pair? strs) (pair? (cdr strs)) (cadr strs))))

;; ---- value lowering ------------------------------------------------
(define (lower-value v)
  (match v
    [(? string? s) s]
    [(? number? n) n]
    [(? code-tok? c) (fsh-code (code-tok-text c) #f)]
    [(list 'code c) (fsh-code (code-text c) #f)]
    [(list 'code c d) (fsh-code (code-text c) d)]
    [(list 'quantity n u) (fsh-quantity n (code-text u) #f)]
    [(list 'quantity n u d) (fsh-quantity n (code-text u) d)]
    [(list 'quantity u) (fsh-quantity #f (code-text u) #f)]
    [(list 'reference r) (fsh-reference r #f)]
    [(list 'reference r d) (fsh-reference r d)]
    [(list 'canonical c) (fsh-canonical c)]
    [(list 'bool b) (string=? b "true")]
    [(list 'ratio a b) (fsh-ratio (lower-value a) (lower-value b))]
    [(list 'name n) n]
    [_ v]))

(define (code-text x) (if (code-tok? x) (code-tok-text x) x))
