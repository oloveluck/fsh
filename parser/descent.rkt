#lang racket/base

;;; descent.rkt — one FSH entity read by recursive descent: its tokens -> the datum brag's parser
;;; of the grammar (grammar.rkt) gives them, in time linear in the tokens, or #f.
;;;
;;; grammar.rkt (brag, ported from SUSHI's FSH.g4) is the definition of what FSH text is, and stays
;;; so: it reports every syntax error, and tests/descent-test.rkt holds this reader to it. What this
;;; reader adds is speed: brag's general parser costs ~70 µs a token and is superlinear in an
;;; entity's length, which is why parse.rkt reads an entity with it rule by rule and remembers each
;;; parse by its shape; this reader takes a whole entity in one pass.
;;;
;;; It only ever ACCEPTS. On any token it does not expect, and on any rule the grammar reads two
;;; ways, it answers #f, and parse.rkt hands the tokens to brag, whose answer — the tree, or the
;;; positioned error — is the one given.
;;;
;;; The shape of the reading. An entity is a header (its keyword, its name, its metadata) and then
;;; its rules, each beginning with a `*` (a STAR token, which no other part of a rule contains). So
;;; each rule is read on its own, between two STARs: every kind of rule the entity allows is tried
;;; on the rule's tokens, and a kind matches only when it reads ALL of them. One kind matching is the
;;; rule; none, or two (`rule` says which rules those are), and that rule is left to brag — which
;;; parse.rkt asks for that rule alone, as it always has (the header and the rule), so the rest of
;;; the entity is still read here. Inside a rule the reading is deterministic: each choice is decided
;;; by the next token or two, and the grammar's few choices between two readings of one value are
;;; made as brag makes them (`value!`, checked for every value form in every place a value stands).
;;;
;;; A Token is a brag token-struct: a symbol type (KW_PROFILE, SEQUENCE, STAR, …) and a value (a
;;; string, a number for NUMBER, a code-tok for CODE).
;;; A Datum is brag's `parse-to-datum` for the grammar's start rule `entity`: rule names as heads,
;;; a token kept by the grammar as its value, a token cut with `/` (KW_PROFILE, STAR, EQUAL, ARROW,
;;; COLON, KW_INSERT …) left out, `@mostAlphaKeywords` spliced into `name` and `path`:
;;;   (entity (profile (name "P") (sdMetadata (parent (name "Patient")))
;;;                    (sdRule (cardRule (path "name") "1..1" (flag "MS")))))
;;; A Cursor is (cursor tokens i fail): the tokens of one part (a header or a rule) as a vector, the
;;; index of the next one (advanced by mutation, local to one reading), and the escape to take when
;;; the part is not what is being read.

(require racket/contract
         racket/match
         (only-in racket/list split-at)
         (only-in brag/support token-struct? token-struct-type token-struct-val))

(provide
 (contract-out
  ;; descend-entity : (listof Token) -> Datum | #f   the entity's datum, or #f when the tokens are
  ;; anything this reader does not take (brag then reads them)
  [descend-entity (-> (listof token-struct?) (or/c #f pair?))]
  ;; descend-parts : (listof Token) -> Parts | #f   the entity's header and each rule, a rule this
  ;; reader does not take as #f (parse.rkt has brag read that rule)
  [descend-parts (-> (listof token-struct?) (or/c #f (cons/c pair? (listof (or/c #f pair?)))))]))

;; ---- the tokens ---------------------------------------------------------------------------------
;; the keywords that are also names and paths (grammar.rkt's @mostAlphaKeywords)
(define most-alpha-keywords
  '(KW_MS KW_SU KW_TU KW_NORMATIVE KW_DRAFT KW_FROM KW_CONTAINS KW_NAMED KW_AND KW_ONLY KW_OR
    KW_OBEYS KW_TRUE KW_FALSE KW_INCLUDE KW_EXCLUDE KW_CODES KW_WHERE KW_VSREFERENCE KW_SYSTEM
    KW_CONTENTREFERENCE))
;; name, path : SEQUENCE | NUMBER | DATETIME | TIME | mostAlphaKeywords
(define name-types (append '(SEQUENCE NUMBER DATETIME TIME) most-alpha-keywords))
(define flag-types '(KW_MOD KW_MS KW_SU KW_TU KW_NORMATIVE KW_DRAFT))
(define strength-types '(KW_EXAMPLE KW_PREFERRED KW_EXTENSIBLE KW_REQUIRED))

;; ---- the cursor ---------------------------------------------------------------------------------
(struct cursor (tokens [i #:mutable] fail))

;; peek : Cursor [Nat] -> Symbol|#f   the type of the token `ahead` places on, #f past the end
(define (peek c [ahead 0])
  (define k (+ (cursor-i c) ahead))
  (define ts (cursor-tokens c))
  (and (< k (vector-length ts)) (token-struct-type (vector-ref ts k))))
;; at-end? : Cursor -> Boolean
(define (at-end? c) (= (cursor-i c) (vector-length (cursor-tokens c))))
;; next! : Cursor -> Value   the next token's value, read
(define (next! c)
  (begin0 (token-struct-val (vector-ref (cursor-tokens c) (cursor-i c)))
          (set-cursor-i! c (add1 (cursor-i c)))))
;; take! : Cursor (listof Symbol) -> Value   the next token, which must be of one of these types
(define (take! c types)
  (if (memq (peek c) types) (next! c) ((cursor-fail c) #f)))
;; fail! : Cursor -> none   this part is not what is being read
(define (fail! c) ((cursor-fail c) #f))
;; finish! : Cursor Datum -> Datum   the datum, when every token was read
(define (finish! c d) (if (at-end? c) d (fail! c)))
;; many! : Cursor (listof Symbol) (Value -> Datum) -> (listof Datum)   the tokens of these types
;; that come next, each made a datum
(define (many! c types f)
  (let loop ()
    (if (memq (peek c) types) (let ([d (f (next! c))]) (cons d (loop))) '())))

;; read-part : (vectorof Token) Nat (Cursor -> Datum) -> Datum | #f   a reading of the tokens from
;; index `start`, or #f when it does not take them
(define (read-part tokens start reader)
  (let/ec fail (reader (cursor tokens start fail))))

;; ---- names, paths, values -----------------------------------------------------------------------
;; name : Cursor -> Datum   (name v)
(define (name! c) (list 'name (take! c name-types)))
;; path : Cursor -> Datum   (path v)
(define (path! c) (list 'path (take! c name-types)))
;; flags : Cursor -> (listof Datum)   flag*  ((flag v) …)
(define (flags! c) (many! c flag-types (lambda (v) (list 'flag v))))

;; value! : Cursor Nat -> Datum   value, which must be exactly the tokens up to index `end`:
;;   STRING | MULTILINE_STRING | NUMBER | DATETIME | TIME | reference | canonical | code | quantity
;;   | ratio | bool | name
;; The grammar reads some of these two ways: a NUMBER, DATETIME or TIME is also a name, KW_TRUE and
;; KW_FALSE are names too, and a CODE (with its display) is also a quantity without a number. brag
;; takes the earlier alternative each time — (value 3), (value (bool "true")), (value (code …)) —
;; and so does this reader (checked by the test).
(define (value! c end)
  (define ts (cursor-tokens c))
  (define start (cursor-i c))
  (define types (for/list ([k (in-range start end)]) (token-struct-type (vector-ref ts k))))
  (define vals (for/list ([k (in-range start end)]) (token-struct-val (vector-ref ts k))))
  (set-cursor-i! c end)
  (define colon (for/first ([t (in-list types)] [k (in-naturals)] #:when (eq? t 'COLON)) k))
  (list 'value
        (match* (types vals)
          [((list (or 'STRING 'MULTILINE_STRING 'NUMBER 'DATETIME 'TIME)) (list v)) v]
          [((list 'REFERENCE) _) (cons 'reference vals)]
          [((list 'REFERENCE 'STRING) _) (cons 'reference vals)]
          [((list 'CANONICAL) _) (cons 'canonical vals)]
          [((or (list 'CODE) (list 'CODE 'STRING)) _) (cons 'code vals)]
          [((list (or 'KW_TRUE 'KW_FALSE)) _) (cons 'bool vals)]
          [((list (? (lambda (t) (memq t name-types)))) _) (cons 'name vals)]
          [(_ _) #:when (quantity-types? types) (cons 'quantity vals)]
          [(_ _) #:when colon
           (define-values (left-types right-types) (split-at types colon))
           (define-values (left-vals right-vals) (split-at vals colon))
           (list 'ratio (ratio-part c left-types left-vals) (ratio-part c (cdr right-types) (cdr right-vals)))]
          [(_ _) (fail! c)])))
;; quantity-types? : (listof Symbol) -> Boolean   quantity : NUMBER? (UNIT | CODE) STRING?
(define (quantity-types? types)
  (match types
    [(list (or 'UNIT 'CODE)) #t]
    [(list (or 'UNIT 'CODE) 'STRING) #t]
    [(list 'NUMBER (or 'UNIT 'CODE)) #t]
    [(list 'NUMBER (or 'UNIT 'CODE) 'STRING) #t]
    [_ #f]))
;; ratio-part : Cursor (listof Symbol) (listof Value) -> Datum   ratioPart : NUMBER | quantity
(define (ratio-part c types vals)
  (cond [(equal? types '(NUMBER)) (cons 'ratioPart vals)]
        [(quantity-types? types) (list 'ratioPart (cons 'quantity vals))]
        [else (fail! c)]))

;; target-type : Cursor -> Datum   targetType : name | referenceType | canonical | codeableReferenceType
(define (target-type! c)
  (list 'targetType
        (case (peek c)
          [(REFERENCE) (list 'referenceType (next! c))]
          [(CANONICAL) (list 'canonical (next! c))]
          [(CODEABLE_REFERENCE) (list 'codeableReferenceType (next! c))]
          [else (name! c)])))
;; element-type : Cursor -> Datum   elementType : SEQUENCE | NUMBER | referenceType | canonical
;;                                                 | codeableReferenceType
(define (element-type! c)
  (list 'elementType
        (case (peek c)
          [(SEQUENCE NUMBER) (next! c)]
          [(REFERENCE) (list 'referenceType (next! c))]
          [(CANONICAL) (list 'canonical (next! c))]
          [(CODEABLE_REFERENCE) (list 'codeableReferenceType (next! c))]
          [else (fail! c)])))
;; ruleset-ref : Cursor -> Datum   RULESET_REFERENCE | paramRuleSetRef
;;   paramRuleSetRef : PARAM_RULESET_REFERENCE parameter* lastParameter
(define (ruleset-ref! c)
  (case (peek c)
    [(RULESET_REFERENCE) (next! c)]
    [(PARAM_RULESET_REFERENCE)
     (define ref (next! c))
     (define params (many! c '(BRACKETED_PARAM PLAIN_PARAM) (lambda (v) (list 'parameter v))))
     (define last (take! c '(LAST_BRACKETED_PARAM LAST_PLAIN_PARAM)))
     (append (list 'paramRuleSetRef ref) params (list (list 'lastParameter last)))]
    [else (fail! c)]))
;; code : Cursor -> Datum   code : CODE STRING?
(define (code! c)
  (define ct (take! c '(CODE)))
  (if (eq? (peek c) 'STRING) (list 'code ct (next! c)) (list 'code ct)))
;; optional-path : Cursor Symbol -> (listof Datum)   `path?` before a token of type `after`: none
;; when that token comes first, else one path (the token types that begin these rules are no path)
(define (optional-path! c after)
  (if (eq? (peek c) after) '() (list (path! c))))
;; short-and-definition : Cursor -> (listof Value)   (STRING (STRING | MULTILINE_STRING)?)?
(define (short-and-definition! c)
  (if (eq? (peek c) 'STRING)
      (let ([short (next! c)])
        (if (memq (peek c) '(STRING MULTILINE_STRING)) (list short (next! c)) (list short)))
      '()))

;; ---- the rules ----------------------------------------------------------------------------------
;; Each reads one rule's tokens after its STAR, all of them, or fails. Rule : its grammar line.

;; cardRule : STAR path CARD flag*
(define (card-rule c)
  (define p (path! c))
  (define card (take! c '(CARD)))
  (finish! c (list* 'cardRule p card (flags! c))))
;; flagRule : STAR path (KW_AND path)* flag+
(define (flag-rule c)
  (define paths (let loop ([acc (list (path! c))])
                  (if (eq? (peek c) 'KW_AND)
                      (let* ([a (next! c)] [p (path! c)]) (loop (list* p a acc)))
                      (reverse acc))))
  (define fs (flags! c))
  (when (null? fs) (fail! c))
  (finish! c (cons 'flagRule (append paths fs))))
;; valueSetRule : STAR path KW_FROM name strength?
(define (value-set-rule c)
  (define p (path! c))
  (define from (take! c '(KW_FROM)))
  (define n (name! c))
  (define s (if (memq (peek c) strength-types) (list (list 'strength (next! c))) '()))
  (finish! c (list* 'valueSetRule p from n s)))
;; fixedValueRule : STAR path EQUAL value KW_EXACTLY?
(define (fixed-value-rule c)
  (define p (path! c))
  (take! c '(EQUAL))
  (define n (vector-length (cursor-tokens c)))
  (define exactly? (and (> n (cursor-i c)) (eq? (token-struct-type (vector-ref (cursor-tokens c) (sub1 n))) 'KW_EXACTLY)))
  (define v (value! c (if exactly? (sub1 n) n)))
  (finish! c (if exactly? (list 'fixedValueRule p v (next! c)) (list 'fixedValueRule p v))))
;; containsRule : STAR path KW_CONTAINS item (KW_AND item)*
(define (contains-rule c)
  (define p (path! c))
  (define kw (take! c '(KW_CONTAINS)))
  (define items (let loop ([acc (list (item! c))])
                  (if (eq? (peek c) 'KW_AND)
                      (let* ([a (next! c)] [it (item! c)]) (loop (list* it a acc)))
                      (reverse acc))))
  (finish! c (list* 'containsRule p kw items)))
;; item : name (KW_NAMED name)? CARD flag*
(define (item! c)
  (define n (name! c))
  (define named (if (eq? (peek c) 'KW_NAMED) (let* ([k (next! c)] [m (name! c)]) (list k m)) '()))
  (define card (take! c '(CARD)))
  (append (list 'item n) named (list card) (flags! c)))
;; onlyRule : STAR path KW_ONLY targetType (KW_OR targetType)*
(define (only-rule c)
  (define p (path! c))
  (define kw (take! c '(KW_ONLY)))
  (define types (let loop ([acc (list (target-type! c))])
                  (if (eq? (peek c) 'KW_OR)
                      (let* ([o (next! c)] [t (target-type! c)]) (loop (list* t o acc)))
                      (reverse acc))))
  (finish! c (list* 'onlyRule p kw types)))
;; obeysRule : STAR path? KW_OBEYS name (KW_AND name)*
;; (`* obeys obeys …` — a path named obeys — is left to brag)
(define (obeys-rule c)
  (when (and (eq? (peek c) 'KW_OBEYS) (eq? (peek c 1) 'KW_OBEYS)) (fail! c))
  (define p (optional-path! c 'KW_OBEYS))
  (define kw (take! c '(KW_OBEYS)))
  (define names (let loop ([acc (list (name! c))])
                  (if (eq? (peek c) 'KW_AND)
                      (let* ([a (next! c)] [n (name! c)]) (loop (list* n a acc)))
                      (reverse acc))))
  (finish! c (append (list 'obeysRule) p (list kw) names)))
;; caretValueRule : STAR path? caretPath EQUAL value
(define (caret-value-rule c)
  (define p (optional-path! c 'CARET_SEQUENCE))
  (define cp (list 'caretPath (take! c '(CARET_SEQUENCE))))
  (take! c '(EQUAL))
  (define v (value! c (vector-length (cursor-tokens c))))
  (finish! c (append (list 'caretValueRule) p (list cp v))))
;; codeCaretValueRule : STAR CODE* caretPath EQUAL value
(define (code-caret-value-rule c)
  (define codes (many! c '(CODE) values))
  (define cp (list 'caretPath (take! c '(CARET_SEQUENCE))))
  (take! c '(EQUAL))
  (define v (value! c (vector-length (cursor-tokens c))))
  (finish! c (append (list 'codeCaretValueRule) codes (list cp v))))
;; mappingRule : STAR path? ARROW STRING (STRING | MULTILINE_STRING)? CODE?
(define (mapping-rule c)
  (define p (optional-path! c 'ARROW))
  (take! c '(ARROW))
  (define target (take! c '(STRING)))
  (define comment (if (memq (peek c) '(STRING MULTILINE_STRING)) (list (next! c)) '()))
  (define code (if (eq? (peek c) 'CODE) (list (next! c)) '()))
  (finish! c (append (list 'mappingRule) p (list target) comment code)))
;; insertRule : STAR path? KW_INSERT (RULESET_REFERENCE | paramRuleSetRef)
(define (insert-rule c)
  (define p (optional-path! c 'KW_INSERT))
  (take! c '(KW_INSERT))
  (finish! c (append (list 'insertRule) p (list (ruleset-ref! c)))))
;; codeInsertRule : STAR CODE* KW_INSERT (RULESET_REFERENCE | paramRuleSetRef)
(define (code-insert-rule c)
  (define codes (many! c '(CODE) values))
  (take! c '(KW_INSERT))
  (finish! c (append (list 'codeInsertRule) codes (list (ruleset-ref! c)))))
;; addCRElementRule : STAR path CARD flag* KW_CONTENTREFERENCE (SEQUENCE | CODE)
;;                    (STRING (STRING | MULTILINE_STRING)?)?
(define (add-cr-element-rule c)
  (define p (path! c))
  (define card (take! c '(CARD)))
  (define fs (flags! c))
  (define kw (take! c '(KW_CONTENTREFERENCE)))
  (define ref (take! c '(SEQUENCE CODE)))
  (finish! c (append (list 'addCRElementRule p card) fs (list kw ref) (short-and-definition! c))))
;; addElementRule : STAR path CARD flag* elementType (/KW_OR elementType)*
;;                  (STRING (STRING | MULTILINE_STRING)?)?
;; (the `or` is cut: kept, it was the first string of the datum and was read as the short)
(define (add-element-rule c)
  (define p (path! c))
  (define card (take! c '(CARD)))
  (define fs (flags! c))
  (define types (let loop ([acc (list (element-type! c))])
                  (if (eq? (peek c) 'KW_OR)
                      (let* ([_ (next! c)] [t (element-type! c)]) (loop (cons t acc)))
                      (reverse acc))))
  (finish! c (append (list 'addElementRule p card) fs types (short-and-definition! c))))
;; pathRule : STAR path
(define (path-rule c) (finish! c (list 'pathRule (path! c))))
;; concept : STAR CODE+ STRING? (STRING | MULTILINE_STRING)?
;; (one STRING is the first optional or the second: the datum is the same)
(define (concept c)
  (define codes (many! c '(CODE) values))
  (when (null? codes) (fail! c))
  (define strings
    (case (peek c)
      [(STRING) (let ([s (next! c)]) (if (memq (peek c) '(STRING MULTILINE_STRING)) (list s (next! c)) (list s)))]
      [(MULTILINE_STRING) (list (next! c))]
      [else '()]))
  (finish! c (append (list 'concept) codes strings)))
;; vsComponent : STAR (KW_INCLUDE | KW_EXCLUDE)? (vsConceptComponent | vsFilterComponent)
(define (vs-component c)
  (define ie (if (memq (peek c) '(KW_INCLUDE KW_EXCLUDE)) (list (next! c)) '()))
  (define body
    (case (peek c)
      ;; vsConceptComponent : code vsComponentFrom?
      [(CODE) (let ([cd (code! c)])
                (if (at-end? c) (list 'vsConceptComponent cd) (list 'vsConceptComponent cd (vs-component-from! c))))]
      ;; vsFilterComponent : KW_CODES vsComponentFrom (KW_WHERE vsFilterList)?
      [(KW_CODES) (let* ([kw (next! c)] [from (vs-component-from! c)])
                    (if (eq? (peek c) 'KW_WHERE)
                        (let ([w (next! c)]) (list 'vsFilterComponent kw from w (vs-filter-list! c)))
                        (list 'vsFilterComponent kw from)))]
      [else (fail! c)]))
  (finish! c (append (list 'vsComponent) ie (list body))))
;; vsComponentFrom : KW_FROM (vsFromSystem (KW_AND vsFromValueset)? | vsFromValueset (KW_AND vsFromSystem)?)
(define (vs-component-from! c)
  (define from (take! c '(KW_FROM)))
  (case (peek c)
    [(KW_SYSTEM)
     (define s (vs-from-system! c))
     (if (eq? (peek c) 'KW_AND)
         (let ([a (next! c)]) (list 'vsComponentFrom from s a (vs-from-valueset! c)))
         (list 'vsComponentFrom from s))]
    [(KW_VSREFERENCE)
     (define v (vs-from-valueset! c))
     (if (eq? (peek c) 'KW_AND)
         (let ([a (next! c)]) (list 'vsComponentFrom from v a (vs-from-system! c)))
         (list 'vsComponentFrom from v))]
    [else (fail! c)]))
;; vsFromSystem : KW_SYSTEM name
(define (vs-from-system! c) (let ([kw (take! c '(KW_SYSTEM))]) (list 'vsFromSystem kw (name! c))))
;; vsFromValueset : KW_VSREFERENCE name (KW_AND name)*
;; (`and system` after its names is a name or the system that follows: left to brag)
(define (vs-from-valueset! c)
  (define kw (take! c '(KW_VSREFERENCE)))
  (define names (let loop ([acc (list (name! c))])
                  (cond [(and (eq? (peek c) 'KW_AND) (eq? (peek c 1) 'KW_SYSTEM)) (fail! c)]
                        [(eq? (peek c) 'KW_AND) (let* ([a (next! c)] [n (name! c)]) (loop (list* n a acc)))]
                        [else (reverse acc)])))
  (list* 'vsFromValueset kw names))
;; vsFilterList : vsFilterDefinition (KW_AND vsFilterDefinition)*
(define (vs-filter-list! c)
  (let loop ([acc (list (vs-filter-definition! c))])
    (if (eq? (peek c) 'KW_AND)
        (let* ([a (next! c)] [d (vs-filter-definition! c)]) (loop (list* d a acc)))
        (cons 'vsFilterList (reverse acc)))))
;; vsFilterDefinition : name vsFilterOperator vsFilterValue?
;;   vsFilterOperator : EQUAL | SEQUENCE      vsFilterValue : code | bool | REGEX | STRING
(define (vs-filter-definition! c)
  (define n (name! c))
  (define op (list 'vsFilterOperator (take! c '(EQUAL SEQUENCE))))
  (define v (case (peek c)
              [(CODE) (list (list 'vsFilterValue (code! c)))]
              [(KW_TRUE KW_FALSE) (list (list 'vsFilterValue (list 'bool (next! c))))]
              [(REGEX STRING) (list (list 'vsFilterValue (next! c)))]
              [else '()]))
  (list* 'vsFilterDefinition n op v))

;; the rule readers by kind
(define rule-readers
  (hasheq 'cardRule card-rule 'flagRule flag-rule 'valueSetRule value-set-rule
          'fixedValueRule fixed-value-rule 'containsRule contains-rule 'onlyRule only-rule
          'obeysRule obeys-rule 'caretValueRule caret-value-rule 'insertRule insert-rule
          'pathRule path-rule 'addElementRule add-element-rule 'addCRElementRule add-cr-element-rule
          'concept concept 'codeCaretValueRule code-caret-value-rule 'codeInsertRule code-insert-rule
          'vsComponent vs-component 'mappingRule mapping-rule))

;; ---- what each entity allows ----------------------------------------------------------------------
;; An Alternative is (cons Kind (listof Symbol)): a rule kind and the rule names wrapping it, the
;; outermost first — sdRule's cardRule inside a RuleSet is (cardRule ruleSetRule sdRule).
;; A RuleContext is (listof Alternative), in the grammar's order.
(define sd-kinds '(cardRule flagRule valueSetRule fixedValueRule containsRule onlyRule obeysRule
                   caretValueRule insertRule pathRule))
;; under : Symbol (listof Symbol) [(listof Symbol)] -> RuleContext   each kind wrapped by `outer`
(define (under outer kinds [inner '()])
  (for/list ([k (in-list kinds)]) (list* k outer inner)))
(define rule-contexts
  (hasheq 'sdRule (under 'sdRule sd-kinds)
          'lrRule (append (under 'lrRule sd-kinds '(sdRule)) (under 'lrRule '(addElementRule addCRElementRule)))
          'instanceRule (under 'instanceRule '(fixedValueRule insertRule pathRule))
          'invariantRule (under 'invariantRule '(fixedValueRule insertRule pathRule))
          'vsRule (under 'vsRule '(vsComponent caretValueRule codeCaretValueRule insertRule codeInsertRule))
          'csRule (under 'csRule '(concept codeCaretValueRule codeInsertRule))
          'ruleSetRule (append (under 'ruleSetRule sd-kinds '(sdRule))
                               (under 'ruleSetRule '(addElementRule addCRElementRule concept codeCaretValueRule
                                                     codeInsertRule vsComponent mappingRule)))
          'mappingEntityRule (under 'mappingEntityRule '(mappingRule insertRule pathRule))))

;; An EntitySpec is (entity-spec Symbol Symbol|#f (listof Symbol) (listof Symbol) Nat Symbol|#f):
;; the rule's name, what wraps its metadata (sdMetadata …), the metadata keywords it allows under that
;; wrapper, those it allows bare (context, characteristics), how many metadata it needs at least, and
;; its rules' context (#f: no rules).
(struct entity-spec (name wrap metadata bare min-metadata rules))
(define entity-specs
  (hasheq 'KW_PROFILE    (entity-spec 'profile 'sdMetadata '(KW_PARENT KW_ID KW_TITLE KW_DESCRIPTION) '() 1 'sdRule)
          'KW_EXTENSION  (entity-spec 'extension 'sdMetadata '(KW_PARENT KW_ID KW_TITLE KW_DESCRIPTION) '(KW_CONTEXT) 0 'sdRule)
          'KW_LOGICAL    (entity-spec 'logical 'sdMetadata '(KW_PARENT KW_ID KW_TITLE KW_DESCRIPTION) '(KW_CHARACTERISTICS) 0 'lrRule)
          'KW_RESOURCE   (entity-spec 'resource 'sdMetadata '(KW_PARENT KW_ID KW_TITLE KW_DESCRIPTION) '() 0 'lrRule)
          'KW_INSTANCE   (entity-spec 'instance 'instanceMetadata '(KW_INSTANCEOF KW_TITLE KW_DESCRIPTION KW_USAGE) '() 0 'instanceRule)
          'KW_INVARIANT  (entity-spec 'invariant 'invariantMetadata '(KW_DESCRIPTION KW_EXPRESSION KW_XPATH KW_SEVERITY) '() 0 'invariantRule)
          'KW_VALUESET   (entity-spec 'valueSet 'vsMetadata '(KW_ID KW_TITLE KW_DESCRIPTION) '() 0 'vsRule)
          'KW_CODESYSTEM (entity-spec 'codeSystem 'csMetadata '(KW_ID KW_TITLE KW_DESCRIPTION) '() 0 'csRule)
          'KW_MAPPING    (entity-spec 'mapping 'mappingMetadata '(KW_ID KW_SOURCE KW_TARGET KW_DESCRIPTION KW_TITLE) '() 0 'mappingEntityRule)))

;; metadata! : Cursor -> Datum   one metadata item, by its keyword:
;;   parent : KW_PARENT name      id : KW_ID name             title : KW_TITLE STRING
;;   description : KW_DESCRIPTION (STRING | MULTILINE_STRING)    expression : likewise
;;   xpath : KW_XPATH STRING      severity : KW_SEVERITY CODE  instanceOf : KW_INSTANCEOF name
;;   usage : KW_USAGE CODE        source : KW_SOURCE name     target : KW_TARGET (STRING | SEQUENCE)
;;   context : KW_CONTEXT contextItem* lastContextItem
;;   characteristics : KW_CHARACTERISTICS CODE_ITEM* LAST_CODE_ITEM
(define (metadata! c)
  (define kw (peek c))
  (next! c)
  (case kw
    [(KW_PARENT) (list 'parent (name! c))]
    [(KW_ID) (list 'id (name! c))]
    [(KW_TITLE) (list 'title (take! c '(STRING)))]
    [(KW_DESCRIPTION) (list 'description (take! c '(STRING MULTILINE_STRING)))]
    [(KW_EXPRESSION) (list 'expression (take! c '(STRING MULTILINE_STRING)))]
    [(KW_XPATH) (list 'xpath (take! c '(STRING)))]
    [(KW_SEVERITY) (list 'severity (take! c '(CODE)))]
    [(KW_INSTANCEOF) (list 'instanceOf (name! c))]
    [(KW_USAGE) (list 'usage (take! c '(CODE)))]
    [(KW_SOURCE) (list 'source (name! c))]
    [(KW_TARGET) (list 'target (take! c '(STRING SEQUENCE)))]
    [(KW_CONTEXT)
     (define items (many! c '(QUOTED_CONTEXT UNQUOTED_CONTEXT) (lambda (v) (list 'contextItem v))))
     (append (list 'context) items (list (list 'lastContextItem (take! c '(LAST_QUOTED_CONTEXT LAST_UNQUOTED_CONTEXT)))))]
    [(KW_CHARACTERISTICS)
     (define items (many! c '(CODE_ITEM) values))
     (append (list 'characteristics) items (list (take! c '(LAST_CODE_ITEM))))]
    [else (fail! c)]))

;; header : Cursor EntitySpec -> (listof Datum)   the entity's name and metadata (the header's
;; tokens, all of them)
(define (header! c spec)
  (define n (name! c))
  (define items
    (let loop ()
      (define kw (peek c))
      (cond [(memq kw (entity-spec-metadata spec)) (let ([m (metadata! c)]) (cons (list (entity-spec-wrap spec) m) (loop)))]
            [(memq kw (entity-spec-bare spec)) (let ([m (metadata! c)]) (cons m (loop)))]
            [else '()])))
  (when (< (length items) (entity-spec-min-metadata spec)) (fail! c))
  (finish! c (cons n items)))

;; ---- the entity ---------------------------------------------------------------------------------
;; parts : (vectorof Token) -> (cons (vectorof Token) (listof (vectorof Token)))   the header (the
;; tokens before the first STAR) and each rule (a STAR and the tokens up to the next)
(define (parts toks)
  (define stars (for/list ([t (in-vector toks)] [k (in-naturals)] #:when (eq? (token-struct-type t) 'STAR)) k))
  (define bounds (append stars (list (vector-length toks))))
  (cons (vector-copy-range toks 0 (car bounds))
        (for/list ([from (in-list stars)] [to (in-list (cdr bounds))]) (vector-copy-range toks from to))))
(define (vector-copy-range v from to) (for/vector #:length (- to from) ([k (in-range from to)]) (vector-ref v k)))

;; rule : (vectorof Token) RuleContext -> Datum | #f   the rule's datum: the one alternative of the
;; context that reads all its tokens; #f when none does (an error) or several do (a rule the grammar
;; reads two ways: `* ^short = "x"` and `* insert R` in a ValueSet or a RuleSet, which are a
;; caretValueRule or a codeCaretValueRule with no code, an insertRule or a codeInsertRule; `* #a` in
;; a RuleSet, a concept or a vsComponent). brag's choice between two readings is the first of its
;; parallel attempts to finish, not the grammar's order (`* insert R` is an insertRule there, `* insert
;; R(x)` a codeInsertRule), so it is left to brag.
(define (rule toks context)
  (define readings
    (for*/list ([alt (in-list context)]
                [d (in-value (read-part toks 1 (hash-ref rule-readers (car alt))))]
                #:when d)
      (foldr list d (cdr alt))))
  (and (= (length readings) 1) (car readings)))

;; A Parts is (cons Head (listof (or/c Datum #f))): the entity's datum without its rules — its rule
;; name, name and metadata, `(profile (name "P") (sdMetadata …))` — and each rule's datum, or #f for a
;; rule left to brag.
;; descend-parts : (listof Token) -> Parts | #f   #f when the header (or the entity as a whole: an
;; Alias with rules, a RuleSet without) is not taken here
;;   entity : alias | profile | extension | invariant | instance | valueSet | codeSystem | ruleSet
;;            | mapping | logical | resource
(define (descend-parts tokens)
  (define toks (list->vector tokens))
  (match-define (cons header rules) (parts toks))
  (define kw (and (> (vector-length header) 0) (token-struct-type (vector-ref header 0))))
  (case kw
    ;; alias : KW_ALIAS name EQUAL (SEQUENCE | CODE)
    [(KW_ALIAS)
     (define head (read-part header 1 (lambda (c)
                                        (define n (name! c))
                                        (take! c '(EQUAL))
                                        (finish! c (list 'alias n (take! c '(SEQUENCE CODE)))))))
     (and head (null? rules) (list head))]
    ;; ruleSet : KW_RULESET RULESET_REFERENCE ruleSetRule+
    [(KW_RULESET)
     (define name (read-part header 1 (lambda (c) (finish! c (take! c '(RULESET_REFERENCE))))))
     (and name (pair? rules) (with-rules (list 'ruleSet name) rules 'ruleSetRule))]
    [else
     (define spec (hash-ref entity-specs kw #f))
     (define items (and spec (read-part header 1 (lambda (c) (header! c spec)))))
     (and items (with-rules (cons (entity-spec-name spec) items) rules (entity-spec-rules spec)))]))
;; with-rules : Head (listof (vectorof Token)) Symbol -> Parts
(define (with-rules head rules context-name)
  (define context (hash-ref rule-contexts context-name))
  (cons head (for/list ([r (in-list rules)]) (rule r context))))

;; descend-entity : (listof Token) -> Datum | #f   the whole entity's datum, when every part of it is
;; taken here
(define (descend-entity tokens)
  (define p (descend-parts tokens))
  (and p (andmap values (cdr p)) (list 'entity (append (car p) (cdr p)))))

(module+ test
  (require rackunit brag/support (only-in "lexer.rkt" tokenize) (only-in "ast.rkt" code-tok))
  (define (code-tok* s) (code-tok s))
  ;; tokens : String -> (listof Token)   the text's tokens, spaces and comments left out
  (define (tokens s)
    (define next (tokenize (open-input-string s)))
    (let loop () (define t (next))
      (cond [(or (void? t) (eof-object? t)) '()] [(token-struct-skip? t) (loop)] [else (cons t (loop))])))
  (define (tree s) (descend-entity (tokens s)))
  (check-equal? (tree "Profile: P\nParent: Patient\nId: p\n* name 1..1 MS\n* gender from VS (required)")
                '(entity (profile (name "P") (sdMetadata (parent (name "Patient"))) (sdMetadata (id (name "p")))
                                  (sdRule (cardRule (path "name") "1..1" (flag "MS")))
                                  (sdRule (valueSetRule (path "gender") "from" (name "VS") (strength "(required)"))))))
  (check-equal? (tree "Alias: $a = http://x") '(entity (alias (name "$a") "http://x")))
  ;; the forms the grammar reads two ways, as brag reads them
  (check-equal? (tree "Instance: i\nInstanceOf: P\n* a = true\n* b = 3 (exactly)\n* c = #x \"d\"\n* d = 1 'mg' : 2")
                `(entity (instance (name "i") (instanceMetadata (instanceOf (name "P")))
                                   (instanceRule (fixedValueRule (path "a") (value (bool "true"))))
                                   (instanceRule (fixedValueRule (path "b") (value 3) "(exactly)"))
                                   (instanceRule (fixedValueRule (path "c") (value (code ,(code-tok* "#x") "d"))))
                                   (instanceRule (fixedValueRule (path "d") (value (ratio (ratioPart (quantity 1 "mg")) (ratioPart 2))))))))
  (check-equal? (tree "CodeSystem: C\n* #a \"A\"\n* #a ^designation.value = \"x\"")
                `(entity (codeSystem (name "C") (csRule (concept ,(code-tok* "#a") "A"))
                                     (csRule (codeCaretValueRule ,(code-tok* "#a") (caretPath "^designation.value") (value "x"))))))
  ;; a rule the grammar reads two ways is left to brag, and the rest of its entity is read here
  (check-equal? (descend-parts (tokens "ValueSet: V\n* ^status = #draft\n* include codes from system S"))
                '((valueSet (name "V")) #f (vsRule (vsComponent "include" (vsFilterComponent "codes" (vsComponentFrom "from" (vsFromSystem "system" (name "S"))))))))
  (check-false (tree "RuleSet: R\n* #a \"d\""))
  ;; what is not FSH, or not taken here, is #f (brag says why)
  (for ([s (in-list '("Profile: P" "RuleSet: R" "Profile: P\nParent: Patient\n* name ^short =" "Alias: A = B\n* x 1..1"
                      "Instance: i\n* a = 3 \"x\"" "RuleSet: R\n* obeys obeys a"))])
    (check-false (tree s) s)))

