#lang racket/base

;;; parse.rkt — the FSH document parser.
;;;
;;; A document is pre-split into entity chunks at top-level declaration keywords (so brag's
;;; single-`entity` start rule can parse each), and parameterized-RuleSet *definitions* are
;;; captured raw (their ANTLR body uses token-set negation brag can't express). The same
;;; parser backs both the embedded API and the `#lang fsh` reader.

(require brag/support
         (only-in fhir/diagnostic raise-diagnostic exn->diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic
                  error-position loc diagnostic diagnostic? attach-loc! diagnostic-loc diagnostic-message diagnostic-lang diagnostic-phase diagnostic-severity loc-line)
         "../lint.rkt"
         racket/contract
         racket/match
         (only-in racket/list append* take last)
         (only-in racket/string string-split string-replace)
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt")

(provide
 (contract-out
  ;; FSH source -> list of entity ASTs ; a failure raises exn:fail:diagnostic (fhir/diagnostic)
  ;; positioned in `source` (a file name for messages)
  [parse-fsh-doc    (->* (string?) (#:source (or/c string? path? #f)) list?)]
  ;; the same, each entity paired with the location of its first token (for compile diagnostics)
  [parse-fsh-doc/located (->* (string?) (#:source (or/c string? path? #f)) (listof pair?))]
  ;; every entity that parses, with its location, and every diagnostic: each entity's parse error
  ;; (the others still parse) and the import-level checks of fsh/lint over the document
  [parse-fsh-doc/diagnostics (->* (string?) (#:source (or/c string? path? #f)) (values (listof pair?) list?))]
  [parse-fsh-entity (-> string? any/c)]))  ; one entity's source -> its AST

(define entity-start-types
  '(KW_ALIAS KW_PROFILE KW_EXTENSION KW_INSTANCE KW_INVARIANT KW_VALUESET
    KW_CODESYSTEM KW_RULESET KW_MAPPING KW_LOGICAL KW_RESOURCE))

(define (collect-tokens str)
  (define next (tokenize (open-input-string str)))
  (let loop ([acc '()])
    (define t (next))
    (if (or (void? t) (eof-object? t)) (reverse acc) (loop (cons t acc)))))

;; probe-token : token|srcloc-token -> token-struct|#f   (skip tokens excluded)
(define (probe-token t)
  (define k (if (srcloc-token? t) (srcloc-token-token t) t))
  (and (token-struct? k) (not (token-struct-skip? k)) k))
;; ran-out-probe : tokens last-position -> (-> boolean)   re-parse with a sentinel token after the
;; last one: if the error moves to the sentinel, the input was a valid prefix that ran out
(define ((ran-out-probe toks last-pos) )
  ;; the sentinel is a COPY of the last real token (a type the grammar knows), placed after it
  (define last-real (for/last ([t (in-list toks)] #:when (probe-token t)) (probe-token t)))
  (and last-pos last-real
       (with-handlers ([exn:fail? (lambda (e) (let ([p (error-position e)]) (and p (> p last-pos))))])
         (g:parse-to-datum (let ([ts (append toks (list (token (token-struct-type last-real) (token-struct-val last-real)
                                                                #:position (+ last-pos 100000) #:line 1 #:column 0)))])
                             (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts)))))))
         #f)))
(define (real-tok? t) (not (token-struct-skip? t)))
(define (ttype t) (token-struct-type t))
(define (tval t) (token-struct-val t))

;; group tokens into per-entity chunks at non-skip entity-start keywords
(define (chunk-entities toks)
  (let loop ([toks toks] [cur '()] [chunks '()])
    (cond
      [(null? toks)
       (reverse (if (null? cur) chunks (cons (reverse cur) chunks)))]
      [(and (real-tok? (car toks)) (memq (ttype (car toks)) entity-start-types) (pair? cur))
       (loop (cdr toks) (list (car toks)) (cons (reverse cur) chunks))]
      [else (loop (cdr toks) (cons (car toks) cur) chunks)])))

(define (parse-fsh-doc str #:source [source #f]) (map car (parse-fsh-doc/located str #:source source)))
(define (parse-fsh-doc/located str #:source [source #f])
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (raise-diagnostic (hinted (exn:fail:diagnostic-diagnostic e) str)))])
    (for/list ([ch (in-list (real-chunks str source))])
      (parse-located-chunk ch source))))
(define (parse-fsh-doc/diagnostics str #:source [source #f])
  (define-values (es ds) (parse-fsh-doc/diagnostics* str #:source source))
  (values es (for/list ([d (in-list ds)]) (hinted d str))))

;; hinted : diagnostic text -> diagnostic   a parse error whose line shows a common slip gets the fix:
;; a number followed by a bare unit (`* valueQuantity = 7.1 %`) — FSH quotes a UCUM unit (`7.1 '%'`)
(define (hinted d str)
  (define l (diagnostic-loc d))
  (define line (and l (loc-line l)
                    (let ([ls (regexp-split #rx"\n" str)]) (and (<= (loc-line l) (length ls)) (list-ref ls (sub1 (loc-line l)))))))
  (define msg (diagnostic-message d))
  (cond
    [(and line (regexp-match? #rx"^unexpected" msg)
          (regexp-match #px"=\\s*(-?[0-9][0-9.]*)\\s+([^'\"\\s#]+)\\s*$" line))
     => (lambda (m)
          (diagnostic (diagnostic-lang d) (diagnostic-phase d) (diagnostic-severity d)
                      (format "~a — a quantity's UCUM unit is quoted: `~a '~a'`" msg (cadr m) (caddr m))
                      l))]
    [else d]))

(define (parse-fsh-doc/diagnostics* str #:source [source #f])
  (define chunks (with-handlers ([exn:fail:diagnostic? exn:fail:diagnostic-diagnostic]) (real-chunks str source)))
  (cond
    [(diagnostic? chunks) (values '() (list chunks))]    ; the lexer refused the text
    [else
     (define parsed      ; (listof (list entity loc tokens)) | a diagnostic per chunk
       (for/list ([ch (in-list chunks)])
         (with-handlers ([exn:fail:diagnostic? exn:fail:diagnostic-diagnostic])
           (define p (parse-located-chunk ch source))
           (list (car p) (cdr p) (filter real-tok? ch)))))
     (define items (filter list? parsed))
     (values (for/list ([it (in-list items)]) (cons (car it) (cadr it)))
             (append (filter diagnostic? parsed) (lint-document items)))]))
;; real-chunks : string source -> (listof tokens)   the entity chunks that hold a real token
(define (real-chunks str source)
  (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                   (lambda (e) (raise-diagnostic (exn->diagnostic e 'fsh #:source source)))])
    (filter (lambda (ch) (ormap real-tok? ch)) (chunk-entities (collect-tokens str)))))
;; parse-located-chunk : tokens source -> (cons entity loc)   raises exn:fail:diagnostic
(define (parse-located-chunk ch source)
  (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                   (lambda (e) (raise-diagnostic (exn->diagnostic e 'fsh #:source source)))])
    ;; a parse that runs out of the entity's tokens reads "the entity ends after `X`"
    (define last-pos (for/last ([t (in-list ch)] #:when (real-tok? t)) (token-struct-offset t)))
    (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                     (lambda (e) (raise-diagnostic (exn->diagnostic e 'fsh #:source source #:last-position last-pos
                                                                    #:unit "entity"
                                                                    #:ran-out? (ran-out-probe (filter real-tok? ch) last-pos))))])
      (define first-real (for/first ([t (in-list ch)] #:when (real-tok? t)) t))
      (cons (parse-chunk ch #:source source) (token-loc first-real source)))))
;; token-loc : token source -> loc
(define (token-loc t source)
  (loc (and source (if (path? source) (path->string source) source))
       (token-struct-line t)
       (let ([c (token-struct-column t)]) (and c (add1 c)))
       (token-struct-offset t) (token-struct-span t)))

;; parse one entity's token chunk
(define (parse-chunk toks #:source [source #f])
  (define reals (filter real-tok? toks))
  (cond
    [(param-ruleset? reals) (capture-param-ruleset reals)]
    [else
     ;; each rule's `*` with the token after it: the indentation, and where the rule is written
     (define starts (for/list ([t (in-list reals)] [nxt (in-list (append (cdr reals) (list #f)))]
                               #:when (eq? (ttype t) 'STAR))
                      (cons t nxt)))
     (define locs (for/list ([st (in-list starts)])
                    (define t (or (cdr st) (car st)))
                    (and (token-struct-line t) (token-loc t source))))
     (define e (apply-indentation (lower-entity (parse-reals reals))
                                  (for/list ([st (in-list starts)] [l (in-list locs)]) (cons (star-indent (car st)) l))))
     ;; each rule remembers where it is written (the token after its `*`), for compile diagnostics
     (define rules (entity-rules e))
     (when (and rules (= (length rules) (length locs)))
       (for ([r (in-list rules)] [l (in-list locs)] #:when l)
         (attach-loc! r l)))
     e]))
;; ---- indented rules (FSH 2.0 "path context") ------------------------------------------------
;; A rule indented under another continues its path: under `* extension[a][0]`, an indented
;; `* extension[b].valueString = "x"` means `extension[a][0].extension[b].valueString`. Each level is
;; two spaces; the context is the nearest preceding rule one level up. A context's `[+]` reads as
;; `[=]` for its children (they continue the element it just added). Concept and code-caret rules take
;; the parent's codes (`* #a` / `  * #b` = `* #a #b`, a child concept).
;; star-indent : STAR token -> columns before the `*` on its line
(define (star-indent t)
  (define line (last (string-split (string-append (tval t) "|") "\n")))
  (string-length (car (regexp-match #px"^[ \t]*" line))))

;; apply-indentation : entity (listof (cons indent loc)) -> entity   one indent per rule, in order
(define (apply-indentation e indents)
  (define (fix rules)
    (if (or (andmap (lambda (i) (zero? (car i))) indents) (not (= (length rules) (length indents))))
        rules
        (contextualize rules indents)))
  (match e
    [(fsh-profile n m r) (fsh-profile n m (fix r))]
    [(fsh-extension n m r) (fsh-extension n m (fix r))]
    [(fsh-logical n m r) (fsh-logical n m (fix r))]
    [(fsh-resource n m r) (fsh-resource n m (fix r))]
    [(fsh-instance n m r) (fsh-instance n m (fix r))]
    [(fsh-valueset n m r) (fsh-valueset n m (fix r))]
    [(fsh-codesystem n m r) (fsh-codesystem n m (fix r))]
    [(fsh-mapping n m r) (fsh-mapping n m (fix r))]
    [(fsh-ruleset n r) (fsh-ruleset n (fix r))]
    [_ e]))

;; contextualize : rules (listof (cons indent loc)) -> rules   stack entries are (list level path codes)
;; The indentation rules SUSHI's importer enforces (FSHImporter.getPathWithContext), each a parse
;; error positioned at the rule: two spaces per level; the first rule left-aligned; at most one level
;; deeper than the rule above; a rule without a path (a value set component) is never indented, and
;; nothing is indented under one — except a code rule under a single-concept component, which takes
;; its code (and then names two concepts, which fsh/lint reports); `.` only at the top level.
(define (contextualize rules indents)
  (define (fail l msg) (raise-diagnostic (diagnostic 'fsh 'parse 'error msg l)))
  (let loop ([rs rules] [is indents] [stack '()] [acc '()] [prev-level 0])
    (cond
      [(null? rs) (reverse acc)]
      [else
       (define r0 (car rs))
       (define spaces (car (car is)))
       (define l (cdr (car is)))
       (when (odd? spaces)
         (fail l (format "Unable to determine path context for the rule: it is indented ~a space~a; rules are indented in increments of two spaces"
                         spaces (if (= spaces 1) "" "s"))))
       (define level (quotient spaces 2))
       (when (and (null? acc) (> level 0)) (fail l "The first rule of a definition must be left-aligned"))
       (when (> level (add1 prev-level)) (fail l "The rule is indented too deeply: a rule is indented at most one level (two spaces) below the rule above it"))
       (when (and (> level 0) (vs-component? r0)) (fail l "A rule that does not use a path cannot be indented"))
       (when (and (> level 0) (equal? (rule-target-path r0) ".")) (fail l "The special '.' path is only allowed in top-level rules"))
       (define live (filter (lambda (s) (< (car s) level)) stack))
       (define ctx (for/first ([s (in-list live)] #:when (= (car s) (sub1 level))) s))
       (when (and ctx (not (cadr ctx)) (not (caddr ctx)))
         (fail l "Rule cannot be indented below rule which has no path"))
       (define r (if ctx (in-context r0 (cadr ctx) (caddr ctx)) r0))
       (loop (cdr rs) (cdr is) (cons (list level (context-path r) (rule-codes r)) live) (cons r acc) level)])))

(define (join-path ctx0 p)
  (define ctx (and ctx0 (regexp-replace #rx"[.]$" ctx0 "")))   ; `* product.` is a context too
  (cond [(not ctx) p]
        [(or (not p) (string=? p "") (string=? p ".")) ctx]
        [(string=? ctx "") p]
        [else (string-append ctx "." p)]))
;; a rule's path as context for the rules under it ([+] -> [=])
(define (context-path r)
  (define p (rule-target-path r))
  (and p (string-replace p "[+]" "[=]")))
(define (rule-codes r)
  (match r
    [(concept-rule cs _ _) cs] [(rule-code-caret cs _ _) cs] [(rule-code-insert cs _) cs]
    [(vs-component _ body) (let ([cs (component-codes body)]) (and (= (length cs) 1) cs))]
    [_ #f]))
;; component-codes : vs-component body -> (listof string)   the concept codes a component names
(define (component-codes body)
  (let walk ([x body])
    (match x
      [(list 'code c _ ...) (list (code-text c))]
      [(? pair?) (append (walk (car x)) (walk (cdr x)))]
      [_ '()])))
(define (code-text c) (if (code-tok? c) (code-tok-text c) c))
(define (in-context r ctx codes)
  (define (jc cs) (if codes (append codes cs) cs))
  (match r
    [(concept-rule cs d df) (concept-rule (jc cs) d df)]
    [(rule-code-caret cs cp v) (rule-code-caret (jc cs) cp v)]
    [(rule-code-insert cs ref) (rule-code-insert (jc cs) ref)]
    [_ (map-rule-path (lambda (p) (join-path ctx p)) r)]))

;; parse-reals : entity tokens -> its datum (entity (<kind> header-item … rule …))
;; brag's CFG parser is superlinear in input length, and real IGs have entities with hundreds of rules
;; (IPS's Composition profile: 105 s as one parse). A long entity is therefore parsed RULE BY RULE:
;; the header alone, then header + each `*` rule group, keeping the rule nodes each parse adds.
;; Same tree, linear cost.
(define rules-per-parse 16)
(define (parse-reals reals)
  (define groups (star-groups reals))
  (cond
    [(<= (length (cdr groups)) rules-per-parse) (g:parse-to-datum reals)]
    [else
     (define header (car groups))
     ;; a RuleSet's header alone (`RuleSet: Name`) is not a valid parse — the grammar wants a rule —
     ;; so its header items are counted from header + the first rule, which adds one rule node
     (define-values (inner n)
       (with-handlers ([exn:fail? (lambda (_)
                                    (match-define (list 'entity one) (g:parse-to-datum (append header (cadr groups))))
                                    (values (take one (sub1 (length one))) (sub1 (length one))))])
         (match-define (list 'entity inner) (g:parse-to-datum header))
         (values inner (length inner))))
     (list 'entity
           (append inner
                   (append* (for/list ([grp (in-list (cdr groups))])
                              (match-define (list 'entity one) (g:parse-to-datum (append header grp)))
                              (list-tail one n)))))]))

;; star-groups : tokens -> (cons header-tokens (listof rule-token-group)), split at STAR tokens
(define (star-groups reals)
  (let loop ([ts reals] [cur '()] [groups '()])
    (cond
      [(null? ts) (let ([all (reverse (cons (reverse cur) groups))]) all)]
      [(eq? (ttype (car ts)) 'STAR) (loop (cdr ts) (list (car ts)) (cons (reverse cur) groups))]
      [else (loop (cdr ts) (cons (car ts) cur) groups)])))

(define (param-ruleset? reals)
  (and (pair? reals) (eq? (ttype (car reals)) 'KW_RULESET)
       (pair? (cdr reals)) (eq? (ttype (cadr reals)) 'PARAM_RULESET_REFERENCE)))

;; KW_RULESET PARAM_RULESET_REFERENCE param* lastParam  then raw body tokens
(define (capture-param-ruleset reals)
  (define name (tval (cadr reals)))
  (define-values (params rest) (splitf-params (cddr reals)))
  (fsh-param-ruleset name params
                     (for/list ([t (in-list rest)]) (cons (ttype t) (tval t)))))

(define (splitf-params toks)
  (let loop ([toks toks] [ps '()])
    (cond
      [(null? toks) (values (reverse ps) '())]
      [(memq (ttype (car toks)) '(BRACKETED_PARAM PLAIN_PARAM))
       (loop (cdr toks) (cons (clean-param (tval (car toks))) ps))]
      [(memq (ttype (car toks)) '(LAST_BRACKETED_PARAM LAST_PLAIN_PARAM))
       (values (reverse (cons (clean-param (tval (car toks))) ps)) (cdr toks))]
      [else (values (reverse ps) toks)])))

(define (clean-param s)
  (regexp-replace* #px"^[\\s\\[]+|[\\]\\s,)]+$" s ""))

;; parse a single entity from source (helper for tests)
(define (parse-fsh-entity str)
  (define ds (parse-fsh-doc str))
  (if (pair? ds) (car ds) (error 'parse-fsh-entity "no entity in: ~s" str)))

(module+ test
  (require rackunit (only-in fhir/diagnostic diagnostic->string exn:fail:diagnostic-diagnostic))
  ;; diagnostics: positioned, in words, "ends after" when the entity runs out
  (define (diag src) (with-handlers ([exn:fail:diagnostic? (lambda (e) (diagnostic->string (exn:fail:diagnostic-diagnostic e)))])
                       (parse-fsh-doc src #:source "t.fsh") #f))
  (check-equal? (diag "Profile: P\nParent: Patient\n* gender = #male ^short used\n")
                "t.fsh:3:18: fsh parse error: unexpected caret sequence `^short`")
  (check-equal? (diag "Profile: P\nParent: Patient\n* name ^short =\n") "t.fsh:3:15: fsh parse error: the entity ends after `=`")
  (check-equal? (diag "Profile: P\nParent: Patient\n* name ^short = = 3\n") "t.fsh:3:17: fsh parse error: unexpected equal `=`")
  (check-equal? (cdr (car (parse-fsh-doc/located "Alias: A = http://x\n\nProfile: P\nParent: Patient\n" #:source "t.fsh")))
                (loc "t.fsh" 1 1 1 6))
  ;; indentation: SUSHI's rules, each a parse error at the rule
  (check-equal? (diag "Profile: P\nParent: Patient\n  * name 1..1\n")
                "t.fsh:3:5: fsh parse error: The first rule of a definition must be left-aligned")
  (check-equal? (diag "Profile: P\nParent: Patient\n* ^url = \"http://x\"\n  * name 1..1\n")
                "t.fsh:4:5: fsh parse error: Rule cannot be indented below rule which has no path")
  (check-equal? (diag "Instance: I\nInstanceOf: Patient\n* name.family = \"f\"\n    * id = \"b\"\n")
                "t.fsh:4:7: fsh parse error: The rule is indented too deeply: a rule is indented at most one level (two spaces) below the rule above it")
  (check-equal? (diag "ValueSet: V\n* include codes from system ZOO\n  * include codes from system X\n")
                "t.fsh:3:5: fsh parse error: A rule that does not use a path cannot be indented")
  (check-equal? (diag "Profile: P\nParent: Patient\n* name 1..1\n  * given 1..1\n") #f)
  ;; every entity that parses, every entity's parse error, and the lint's findings
  (let-values ([(es ds) (parse-fsh-doc/diagnostics
                         "Profile: P\nParent: Patient\n\nProfile: P\nParent: Observation\n\nProfile: Q\nParent: Patient\n* name ^short =\n"
                         #:source "t.fsh")])
    (check-equal? (length es) 2)
    (check-equal? (map diagnostic->string ds)
                  '("t.fsh:9:15: fsh parse error: the entity ends after `=`"
                    "t.fsh:4:1: fsh lint error: Profile named P already exists"))))
