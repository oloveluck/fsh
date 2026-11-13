#lang racket/base

;;; lint.rkt — the import-level checks SUSHI's importer makes on a parsed document, as diagnostics.
;;;
;;; The grammar decides what parses; SUSHI's importer then rejects documents that parse but cannot be
;;; meant: two entities of one kind with one name, an alias redefined to another value, a metadata
;;; keyword declared twice, an Instance without its InstanceOf or with an unknown Usage, a cardinality
;;; with neither side, an added element missing its min, max or short, a value that offers a choice
;;; of references, a concept in a CodeSystem that names a system, several concepts before a caret or
;;; insert rule in a ValueSet, a filter value of the wrong kind, and an operator written without the
;;; spaces around it. Each is an error positioned where it is written. The oracle is SUSHI's own
;;; importer test suite (tests/sushi-importer-test.rkt).

(require racket/match racket/list racket/string racket/contract
         brag/support
         (only-in fhir/diagnostic diagnostic diagnostic? loc loc-source attached-loc)
         "parser/ast.rkt")

(provide
 (contract-out
  ;; A Located is (list entity loc tokens): a parsed entity, where it starts, its real tokens
  [lint-document (-> (listof (list/c any/c any/c list?)) (listof diagnostic?))]))

;; lint-document : (listof Located) -> (listof diagnostic)
(define (lint-document items)
  (append (duplicate-names items)
          (alias-redefinitions items)
          (append* (for/list ([it (in-list items)]) (apply lint-entity it)))))

;; err : loc string -> diagnostic
(define (err l msg) (diagnostic 'fsh 'lint 'error msg l))

;; ---- the document ------------------------------------------------------------------------------
;; entity-kind : entity -> string|#f   SUSHI's name for the kind, as its "already exists" messages say it
(define (entity-kind e)
  (match e
    [(? fsh-profile?) "Profile"] [(? fsh-extension?) "Extension"] [(? fsh-logical?) "Logical Model"]
    [(? fsh-resource?) "Resource"] [(? fsh-instance?) "Instance"] [(? fsh-invariant?) "Invariant"]
    [(? fsh-valueset?) "ValueSet"] [(? fsh-codesystem?) "code system"] [(? fsh-mapping?) "Mapping"]
    [(or (? fsh-ruleset?) (? fsh-param-ruleset?)) "RuleSet"]
    [_ #f]))
;; entity-name : entity -> string|#f
(define (entity-name e)
  (match e
    [(fsh-profile n _ _) n] [(fsh-extension n _ _) n] [(fsh-logical n _ _) n] [(fsh-resource n _ _) n]
    [(fsh-instance n _ _) n] [(fsh-invariant n _ _) n] [(fsh-valueset n _ _) n] [(fsh-codesystem n _ _) n]
    [(fsh-mapping n _ _) n] [(fsh-ruleset n _) n] [(fsh-param-ruleset n _ _) n] [(fsh-alias n _) n]
    [_ #f]))

;; duplicate-names : (listof Located) -> (listof diagnostic)   a second entity of a kind with a name
(define (duplicate-names items)
  (define seen (make-hash))
  (for*/list ([it (in-list items)]
              [kind (in-value (entity-kind (car it)))] #:when kind
              [name (in-value (entity-name (car it)))] #:when name
              #:unless (begin0 (not (hash-ref seen (cons kind name) #f)) (hash-set! seen (cons kind name) #t)))
    (err (cadr it) (format "~a named ~a already exists" kind name))))

;; alias-redefinitions : (listof Located) -> (listof diagnostic)   the same alias with another value
(define (alias-redefinitions items)
  (define values-of (make-hash))
  (for*/list ([it (in-list items)] #:when (fsh-alias? (car it))
              [a (in-value (car it))]
              [prior (in-value (hash-ref values-of (fsh-alias-name a) #f))]
              #:unless (begin0 (or (not prior) (equal? prior (fsh-alias-value a)))
                               (unless prior (hash-set! values-of (fsh-alias-name a) (fsh-alias-value a)))))
    (err (cadr it) (format "Alias ~a cannot be redefined to ~a; it is already defined as ~a"
                           (fsh-alias-name a) (fsh-alias-value a) prior))))

;; ---- one entity --------------------------------------------------------------------------------
;; lint-entity : entity loc tokens -> (listof diagnostic)
(define (lint-entity e l toks)
  (define src (and l (loc-source l)))
  (append (duplicate-metadata toks src)
          (unquoted-target toks src)
          (operator-spacing e toks src)
          (instance-checks e (or (keyword-loc toks 'KW_USAGE src) l))
          (append* (for/list ([r (in-list (or (entity-rules e) '()))])
                     (lint-rule e r (or (attached-loc r) l))))))

(define metadata-keywords
  '(KW_PARENT KW_ID KW_TITLE KW_DESCRIPTION KW_EXPRESSION KW_XPATH KW_SEVERITY KW_USAGE KW_INSTANCEOF
    KW_SOURCE KW_TARGET KW_CONTEXT KW_CHARACTERISTICS))
;; duplicate-metadata : tokens source -> (listof diagnostic)   a metadata keyword declared twice (before the rules)
(define (duplicate-metadata toks src)
  (define header (let loop ([ts toks]) (cond [(null? ts) '()] [(eq? (token-struct-type (car ts)) 'STAR) '()]
                                             [else (cons (car ts) (loop (cdr ts)))])))
  (define seen (make-hasheq))
  (for/list ([t (in-list header)]
             #:when (memq (token-struct-type t) metadata-keywords)
             #:unless (begin0 (not (hash-ref seen (token-struct-type t) #f)) (hash-set! seen (token-struct-type t) #t)))
    (err (token-loc t src) (format "The ~a metadata is declared more than once"
                               (string-trim (regexp-replace #rx":$" (string-trim (token-struct-val t)) ""))))))

;; unquoted-target : tokens source -> (listof diagnostic)   a Mapping's `Target:` is a quoted string
;; (FSH.g4: `target: KW_TARGET STRING`). The grammar here reads a bare word too, as SUSHI's parser
;; recovers from one, so that the mistake is said where it is rather than as a failed parse.
(define (unquoted-target toks src)
  (for/list ([t (in-list toks)] [next (in-list (if (null? toks) '() (cdr toks)))]
             #:when (and (eq? (token-struct-type t) 'KW_TARGET) (eq? (token-struct-type next) 'SEQUENCE)))
    (err (token-loc next src)
         (format "The Target of a Mapping is a quoted string: write `Target: \"~a\"`" (token-struct-val next)))))

;; operator-spacing : entity tokens source -> (listof diagnostic)   `=`/`->` need a space on both sides
;; (SUSHI's lexer reads `a=b` as one word and its error listener explains the failure this way)
(define (operator-spacing e toks src)
  (define (end t) (+ (token-struct-offset t) (token-struct-span t)))
  (for/list ([prev (in-list (cons #f toks))] [t (in-list toks)]
             [next (in-list (if (null? toks) '() (append (cdr toks) (list #f))))]
             #:when (memq (token-struct-type t) '(EQUAL ARROW))
             ;; (a `*` token carries its trailing space: `* -> "x"` is spaced)
             #:when (or (and prev (not (eq? (token-struct-type prev) 'STAR)) (eqv? (end prev) (token-struct-offset t)))
                        (and next (eqv? (end t) (token-struct-offset next)))))
    (err (token-loc t src)
         (cond [(eq? (token-struct-type t) 'ARROW)
                "Mapping rules must include at least one space both before and after the '->' operator"]
               [(fsh-alias? e) "Alias declarations must include at least one space both before and after the '=' sign"]
               [else "Assignment rules must include at least one space both before and after the '=' sign"]))))

;; keyword-loc : tokens symbol source -> loc|#f   where a metadata keyword is written
(define (keyword-loc toks type src)
  (for/first ([t (in-list toks)] #:when (eq? (token-struct-type t) type)) (token-loc t src)))
;; instance-checks : entity loc -> (listof diagnostic)   InstanceOf present; Usage one of the three
;; (a system on the usage code, `badsystem#example`, is SUSHI's warning, not an error: ignored here)
(define (instance-checks e l)
  (match e
    [(fsh-instance name meta _)
     (define usage (let ([u (assq 'usage meta)]) (and u (regexp-replace #rx"^[^#]*#" (cdr u) ""))))
     (append
      (if (assq 'instanceOf meta) '() (list (err l (format "Instance ~a is missing its InstanceOf declaration" name))))
      (if (or (not usage) (member usage '("example" "definition" "inline"))) '()
          (list (err l "Invalid Usage. Supported usage codes are \"#example\", \"#definition\", and \"#inline\""))))]
    [_ '()]))

;; lint-rule : entity rule loc -> (listof diagnostic)
(define (lint-rule e r l)
  (append (path-checks e r l) (code-checks r l) (rule-checks e r l)))
;; path-checks : entity rule loc -> (listof diagnostic)   an operator glued to the path (`a->"b"`,
;; `a=b` lex as one word) — SUSHI's spacing rule again
(define (path-checks e r l)
  (define p (rule-target-path r))
  (define bare (and p (regexp-replace* #px"\\[[^\\]]*\\]" p "")))
  (cond [(not bare) '()]
        [(regexp-match? #rx"->" bare)
         (list (err l "Mapping rules must include at least one space both before and after the '->' operator"))]
        [(regexp-match? #rx"=" bare)
         (list (err l "Assignment rules must include at least one space both before and after the '=' sign"))]
        [else '()]))
;; code-checks : rule loc -> (listof diagnostic)   a concept string may not start with whitespace
(define (code-checks r l)
  (define codes
    (match r
      [(concept-rule cs _ _) cs] [(rule-code-caret cs _ _) cs] [(rule-code-insert cs _) cs]
      [(rule-assign _ (fsh-code c _) _) (list c)]
      [(vs-component _ body) (component-codes body)]
      [_ '()]))
  (for/list ([c (in-list codes)] #:when (regexp-match? #px"#\"\\s" c))
    (err l (format "A concept string cannot start with whitespace (~a)" c))))
;; component-codes : vs-component body -> (listof string)   the concepts a component lists (not a
;; filter's code value)
(define (component-codes body)
  (for*/list ([cc (in-list (find-nodes body 'vsConceptComponent))]
              [n (in-list (find-nodes (cdr cc) 'code))])
    (code-text (cadr n))))
(define (code-text c) (if (code-tok? c) (code-tok-text c) (format "~a" c)))
(define pipes-message "Using '|' to list references is no longer supported. Use 'or' to list multiple references")
;; rule-checks : entity rule loc -> (listof diagnostic)
(define (rule-checks e r l)
  (match r
    [(rule-card p card _)
     (if (equal? (string-trim card) "..")
         (list (err l (format "Neither side of the cardinality was specified on path \"~a\". A min, max, or both need to be specified" p)))
         '())]
    [(rule-only _ types)
     (for/list ([t (in-list types)] #:when (and (string? t) (piped-reference? t))) (err l pipes-message))]
    [(rule-assign _ (fsh-reference target _) _)
     #:when (piped-reference? target)
     (list (err l pipes-message))]
    [(rule-caret p _ _)
     #:when (and p (not (equal? p ".")) (or (fsh-valueset? e) (fsh-codesystem? e)))
     (list (err l (format "A caret rule on a ~a takes no path (~a): write `* ^…` or `* <code> ^…`"
                          (if (fsh-valueset? e) "ValueSet" "CodeSystem") p)))]
    [(or (rule-add-element p card _ _ short _) (rule-add-cr-element p card _ _ short _))
     (append
      (if (regexp-match? #px"^\\s*\\.\\." card)
          (list (err l (format "The 'min' cardinality attribute in AddElementRule for path '~a' must be specified" p))) '())
      (if (regexp-match? #px"\\.\\.\\s*$" card)
          (list (err l (format "The 'max' cardinality attribute in AddElementRule for path '~a' must be specified" p))) '())
      (if short '() (list (err l (format "The 'short' attribute in AddElementRule for path '~a' must be specified" p)))))]
    [(rule-assign _ (fsh-reference target _) _)
     (if (choice? target) (list (err l "Multiple choices of references are not allowed when setting a value")) '())]
    [(rule-assign _ (fsh-canonical target) _)
     (if (choice? target) (list (err l "Multiple choices of canonicals are not allowed when setting a value")) '())]
    [(concept-rule codes _ _)
     #:when (fsh-codesystem? e)
     (for/list ([c (in-list codes)] #:when (regexp-match? #px"^[^#]+#" c))
       (err l (format "A concept in a CodeSystem cannot declare its system (~a): write the code alone" c)))]
    [(rule-code-caret codes _ _)
     #:when (and (fsh-valueset? e) (> (length codes) 1))
     (list (err l "Only one concept may be listed before a caret rule on a ValueSet."))]
    [(rule-code-insert codes _)
     #:when (and (fsh-valueset? e) (> (length codes) 1))
     (list (err l "Only one concept may be listed before an insert rule on a ValueSet."))]
    [(vs-component _ body) (append (component-checks body l) (filter-value-checks body l))]
    [_ '()]))

;; component-checks : vs-component body loc -> (listof diagnostic)   a concept component names its
;; system once (`SYS#code` or `from system`, not neither, not both); a filter component has a system
(define (component-checks body l)
  (define systems (find-nodes body 'vsFromSystem))
  (define codes (component-codes body))
  (define with-system (filter (lambda (c) (regexp-match? #px"^[^#]+#" c)) codes))
  (define filters (find-nodes body 'vsFilterDefinition))
  (append
   (if (and (pair? codes) (null? systems) (null? with-system))
       (list (err l "A concept component must name its system: `SYS#code` or `from system SYS`")) '())
   (if (and (pair? with-system) (pair? systems))
       (list (err l "A concept component names its system more than once (in the code and after `from system`)")) '())
   (if (and (pair? filters) (null? systems))
       (list (err l "A filter component must name a system (`codes from system SYS where …`)")) '())))

;; piped-reference? : string -> boolean   `Reference(A | B)`: a `|` beside whitespace lists targets,
;; the syntax FSH dropped. A `|` inside a name is its version (`Reference(USCorePatientProfile|7.0.0)`,
;; as in a Canonical): FSHLexer.g4's REFERENCE holds SEQUENCEs, and a SEQUENCE is any run without
;; whitespace, so SUSHI reads the pin and reports only the spaced form (its error listener's
;; "extraneous input '|'"). HRex pins its US Core targets this way.
(define (piped-reference? t) (regexp-match? #px"^(?:Codeable)?Reference\\s*\\([^)]*(?:\\s[|]|[|]\\s)" t))
;; choice? : string -> boolean   `Reference(A or B)` — a choice of targets inside the parentheses
(define (choice? target) (regexp-match? #px"\\((?:[^)]*\\s)?or\\s[^)]*\\)" target))

;; ---- value set filters -------------------------------------------------------------------------
;; A FilterValueKind is one of: 'code | 'string | 'regex | 'boolean | 'other
;; filter-value-kind : vsFilterValue datum -> FilterValueKind
(define (filter-value-kind v)
  (match v
    [(list 'vsFilterValue (list 'code _ ...)) 'code]
    [(list 'vsFilterValue (list 'regex _)) 'regex]
    [(list 'vsFilterValue (list 'bool _)) 'boolean]
    [(list 'vsFilterValue (? boolean?)) 'boolean]
    [(list 'vsFilterValue (? string? s)) (if (regexp-match? #px"^/.*/$" s) 'regex 'string)]
    [_ 'other]))
;; filter-value-checks : vs-component body loc -> (listof diagnostic)   each filter's value kind
(define (filter-value-checks body l)
  (for*/list ([d (in-list (find-nodes body 'vsFilterDefinition))]
              [msg (in-value (filter-definition-problem d))] #:when msg)
    (err l msg)))
(define filter-operators
  '("=" "is-a" "descendent-of" "descendant-of" "is-not-a" "regex" "in" "not-in" "generalizes" "child-of" "descendent-leaf" "exists"))
;; filter-definition-problem : vsFilterDefinition datum -> string|#f
(define (filter-definition-problem d)
  (match d
    [(list 'vsFilterDefinition _ (list 'vsFilterOperator op))
     (cond [(not (member op filter-operators)) (format "\"~a\" is not a filter operator" op)]
           [(equal? op "exists") #f]
           [else (format "The \"~a\" filter needs a value" op)])]
    [(list 'vsFilterDefinition _ (list 'vsFilterOperator op) v)
     (define kind (filter-value-kind v))
     (cond
       [(not (member op filter-operators)) (format "\"~a\" is not a filter operator" op)]
       [(member op '("=" "is-a" "descendent-of" "descendant-of" "is-not-a" "in" "not-in" "generalizes" "child-of" "descendent-leaf"))
        (and (not (memq kind '(code string))) (format "The \"~a\" filter value must be a code or string" op))]
       [(equal? op "regex")
        (and (not (memq kind '(regex string))) (format "The \"~a\" filter value must be a regex or string" op))]
       [(equal? op "exists")
        (and (not (or (eq? kind 'boolean)
                      (and (eq? kind 'string) (member (cadr v) '("true" "false")))))
             "The \"exists\" filter value must be a boolean value or a string value of \"true\" or \"false\"")]
       [else #f])]
    [_ #f]))
;; find-nodes : datum symbol -> (listof datum)   every sub-datum headed by the tag
(define (find-nodes tree tag)
  (cond [(and (pair? tree) (eq? (car tree) tag)) (list tree)]
        [(pair? tree) (append (find-nodes (car tree) tag) (find-nodes (cdr tree) tag))]
        [else '()]))

;; token-loc : token source -> loc
(define (token-loc t src)
  (loc src (token-struct-line t) (let ([c (token-struct-column t)]) (and c (add1 c)))
       (token-struct-offset t) (token-struct-span t)))

(module+ test
  (require rackunit (only-in fhir/diagnostic diagnostic-message diagnostic-loc loc-line))
  (define (tok type val offset line col span) (token-struct type val offset line col span #f))
  (define (msgs ds) (map diagnostic-message ds))
  ;; two profiles of one name; the second is the duplicate
  (define p1 (fsh-profile "P" '((parent . "Patient")) '()))
  (define p2 (fsh-profile "P" '((parent . "Observation")) '()))
  (check-equal? (msgs (lint-document (list (list p1 (loc #f 1 1 0 8) '()) (list p2 (loc #f 4 1 30 8) '()))))
                '("Profile named P already exists"))
  (check-equal? (diagnostic-loc (car (lint-document (list (list p1 (loc #f 1 1 0 8) '()) (list p2 (loc #f 4 1 30 8) '())))))
                (loc #f 4 1 30 8))
  ;; an alias redefined to another value; the same value again is fine
  (define a1 (fsh-alias "A" "http://x")) (define a2 (fsh-alias "A" "http://y"))
  (check-equal? (msgs (lint-document (list (list a1 (loc #f 1 1 0 6) '()) (list a2 (loc #f 2 1 20 6) '()))))
                '("Alias A cannot be redefined to http://y; it is already defined as http://x"))
  (check-equal? (lint-document (list (list a1 (loc #f 1 1 0 6) '()) (list a1 (loc #f 2 1 20 6) '()))) '())
  ;; duplicate metadata, reported at the second keyword
  (define ds (lint-document (list (list p1 (loc #f 1 1 0 8)
                                        (list (tok 'KW_PROFILE "Profile:" 0 1 0 8) (tok 'SEQUENCE "P" 9 1 9 1)
                                              (tok 'KW_TITLE "Title:" 11 2 0 6) (tok 'STRING "a" 18 2 7 3)
                                              (tok 'KW_TITLE "Title:" 22 3 0 6) (tok 'STRING "b" 29 3 7 3))))))
  (check-equal? (msgs ds) '("The Title metadata is declared more than once"))
  (check-equal? (loc-line (diagnostic-loc (car ds))) 3)
  ;; `=` without spaces
  (check-equal? (msgs (lint-document (list (list p1 (loc #f 1 1 0 8)
                                                 (list (tok 'STAR "* " 0 3 0 2) (tok 'SEQUENCE "status" 2 3 2 6)
                                                       (tok 'EQUAL "=" 8 3 8 1) (tok 'CODE "#final" 10 3 10 6))))))
                '("Assignment rules must include at least one space both before and after the '=' sign"))
  (check-equal? (lint-document (list (list p1 (loc #f 1 1 0 8)
                                           (list (tok 'STAR "* " 0 3 0 2) (tok 'SEQUENCE "status" 2 3 2 6)
                                                 (tok 'EQUAL "=" 9 3 9 1) (tok 'CODE "#final" 11 3 11 6)))))
                '())
  (check-equal? (lint-document (list (list (fsh-mapping "M" '() '()) (loc #f 1 1 0 8)
                                           (list (tok 'STAR "* " 0 3 0 2) (tok 'ARROW "->" 2 3 2 2) (tok 'STRING "x" 5 3 5 3)))))
                '())
  ;; a Mapping's Target written bare (FSH.g4 takes a STRING there)
  (check-equal? (msgs (lint-document (list (list (fsh-mapping "M" '((source . "P") (target . "http://x")) '()) (loc #f 1 1 0 8)
                                                 (list (tok 'KW_TARGET "Target:" 20 3 0 7) (tok 'SEQUENCE "http://x" 28 3 8 8))))))
                '("The Target of a Mapping is a quoted string: write `Target: \"http://x\"`"))
  (check-equal? (msgs (lint-document (list (list (fsh-mapping "M" '((source . "P") (target . "http://x")) '()) (loc #f 1 1 0 8)
                                                 (list (tok 'KW_TARGET "Target:" 20 3 0 7) (tok 'STRING "http://x" 28 3 8 10))))))
                '())
  ;; an Instance without InstanceOf, and with an unknown Usage
  (check-equal? (msgs (lint-document (list (list (fsh-instance "I" '((title . "t")) '()) (loc #f 1 1 0 9) '()))))
                '("Instance I is missing its InstanceOf declaration"))
  (check-equal? (msgs (lint-document (list (list (fsh-instance "I" '((instanceOf . "Patient") (usage . "#baduse")) '()) (loc #f 1 1 0 9) '()))))
                '("Invalid Usage. Supported usage codes are \"#example\", \"#definition\", and \"#inline\""))
  ;; rules: cardinality with neither side; an added element missing min / max / short; a choice of references
  (define (rule-msgs e) (msgs (lint-document (list (list e (loc #f 1 1 0 8) '())))))
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-card "category" ".." '()))))
                '("Neither side of the cardinality was specified on path \"category\". A min, max, or both need to be specified"))
  (check-equal? (rule-msgs (fsh-logical "L" '() (list (rule-add-element "a" "..*" '() '("string") "s" #f)
                                                      (rule-add-element "b" "0.." '() '("string") "s" #f)
                                                      (rule-add-element "c" "1..1" '() '("Address") #f #f))))
                '("The 'min' cardinality attribute in AddElementRule for path 'a' must be specified"
                  "The 'max' cardinality attribute in AddElementRule for path 'b' must be specified"
                  "The 'short' attribute in AddElementRule for path 'c' must be specified"))
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-assign "basedOn" (fsh-reference "Reference(A or B)" #f) #f)
                                                      (rule-assign "subject" (fsh-reference "Reference(Patient)" #f) #f))))
                '("Multiple choices of references are not allowed when setting a value"))
  ;; a CodeSystem concept with a system; two concepts before a ValueSet caret rule
  (check-equal? (rule-msgs (fsh-codesystem "CS" '() (list (concept-rule '("#goat") #f #f) (concept-rule '("ZOO#bat") #f #f))))
                '("A concept in a CodeSystem cannot declare its system (ZOO#bat): write the code alone"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (rule-code-caret '("ZOO#a" "ZOO#b") "^designation.value" "x"))))
                '("Only one concept may be listed before a caret rule on a ValueSet."))
  ;; filter values by kind
  (define (filt op v) (list 'vsFilterComponent "codes" (list 'vsComponentFrom "from" (list 'vsFromSystem "system" (list 'name "ZOO")))
                            (list 'vsFilterList (list 'vsFilterDefinition (list 'name "concept") (list 'vsFilterOperator op) (list 'vsFilterValue v)))))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "is-a" "/bear/"))))))
                '("The \"is-a\" filter value must be a code or string"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "is-a" (list 'code (code-tok "#bear"))))))))
                '())
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "regex" (list 'code (code-tok "#Dog"))))))))
                '("The \"regex\" filter value must be a regex or string"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "exists" "/true/"))))))
                '("The \"exists\" filter value must be a boolean value or a string value of \"true\" or \"false\""))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "exists" (list 'bool "true")))))))
                '())
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "in" (list 'bool "true")))))))
                '("The \"in\" filter value must be a code or string"))
  ;; components: a system once; filters with a system, a known operator, a value
  (define (sys name) (list 'vsComponentFrom "from" (list 'vsFromSystem "system" (list 'name name))))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsConceptComponent (list 'code (code-tok "#hippo"))))))))
                '("A concept component must name its system: `SYS#code` or `from system SYS`"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsConceptComponent (list 'code (code-tok "ZOO#hippo")) (sys "ZOO")))))))
                '("A concept component names its system more than once (in the code and after `from system`)"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsConceptComponent (list 'code (code-tok "#hippo")) (sys "ZOO")))))))
                '())
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsFilterComponent "codes" (sys "ZOO") (list 'vsFilterList (list 'vsFilterDefinition (list 'name "display") (list 'vsFilterOperator "resembles") (list 'vsFilterValue "cat")))))))))
                '("\"resembles\" is not a filter operator"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsFilterComponent "codes" (sys "ZOO") (list 'vsFilterList (list 'vsFilterDefinition (list 'name "display") (list 'vsFilterOperator "regex")))))))))
                '("The \"regex\" filter needs a value"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (list 'vsFilterComponent "codes" (list 'vsFilterList (list 'vsFilterDefinition (list 'name "version") (list 'vsFilterOperator "=") (list 'vsFilterValue "2.0")))))))))
                '("A filter component must name a system (`codes from system SYS where …`)"))
  ;; pipes in references; a caret rule with a path on a ValueSet; a concept string starting with whitespace; a glued operator
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-only "performer" '("Reference(Organization | CareTeam)")))))
                (list pipes-message))
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-only "instantiates" '("Canonical(A|1.0 or B|2.0)"))))) '())
  ;; a filter's code value is not a concept of the component
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (vs-component #t (list (filt "is-a" (list 'code (code-tok "ZOO#bear")))))))) '())
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-assign "subject" (fsh-reference "Reference(A |B)" #f) #f))))
                (list pipes-message))
  ;; a `|` with no whitespace beside it pins a version (SUSHI's lexer reads it as part of the name)
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-only "patient" '("Reference(USCorePatientProfile|7.0.0)"))
                                                      (rule-only "performer" '("Reference(USCorePatientProfile|7.0.0 or RelatedPerson)"))
                                                      (rule-assign "subject" (fsh-reference "Reference(A|1.0)" #f) #f))))
                '())
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (rule-caret "mypath" "^publisher" "foo"))))
                '("A caret rule on a ValueSet takes no path (mypath): write `* ^…` or `* <code> ^…`"))
  (check-equal? (rule-msgs (fsh-valueset "VS" '() (list (rule-caret #f "^publisher" "foo")))) '())
  (check-equal? (rule-msgs (fsh-profile "P" '() (list (rule-assign "code" (fsh-code "#\" Leading\"" #f) #f))))
                '("A concept string cannot start with whitespace (#\" Leading\")"))
  (check-equal? (rule-msgs (fsh-mapping "M" '() (list (rule-path "identifier->\"Patient.identifier\""))))
                '("Mapping rules must include at least one space both before and after the '->' operator"))
  (check-equal? (rule-msgs (fsh-instance "I" '((instanceOf . "Patient")) (list (rule-assign "name[=].given" "x" #f)))) '())
  ;; a usage with a system is not an error; the usage error sits on the Usage line
  (check-equal? (msgs (lint-document (list (list (fsh-instance "I" '((instanceOf . "Patient") (usage . "badsystem#example")) '()) (loc #f 1 1 0 9) '()))))
                '()))
