#lang racket/base

;;; parse.rkt — the FSH document parser.
;;;
;;; A document is pre-split into entity chunks at top-level declaration keywords (so brag's
;;; single-`entity` start rule can parse each), and parameterized-RuleSet *definitions* are
;;; captured raw (their ANTLR body uses token-set negation brag can't express). The same
;;; parser backs both the embedded API and the `#lang fsh` reader.

(require brag/support
         (only-in fhir/diagnostic raise-diagnostic exn->diagnostic exn:fail:diagnostic? error-position loc)
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
  (with-handlers ([exn:fail? (lambda (e) (raise-diagnostic (exn->diagnostic e 'fsh #:source source)))])
    (for/list ([ch (in-list (chunk-entities (collect-tokens str)))]
               #:when (ormap real-tok? ch))
      ;; a parse that runs out of the entity's tokens reads "the entity ends after `X`"
      (define last-pos (for/last ([t (in-list ch)] #:when (real-tok? t)) (token-struct-offset t)))
      (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                       (lambda (e) (raise-diagnostic (exn->diagnostic e 'fsh #:source source #:last-position last-pos
                                                                      #:unit "entity"
                                                                      #:ran-out? (ran-out-probe (filter real-tok? ch) last-pos))))])
        (define first-real (for/first ([t (in-list ch)] #:when (real-tok? t)) t))
        (cons (parse-chunk ch)
              (loc (and source (if (path? source) (path->string source) source))
                   (token-struct-line first-real)
                   (let ([c (token-struct-column first-real)]) (and c (add1 c)))
                   (token-struct-offset first-real) (token-struct-span first-real)))))))

;; parse one entity's token chunk
(define (parse-chunk toks)
  (define reals (filter real-tok? toks))
  (cond
    [(param-ruleset? reals) (capture-param-ruleset reals)]
    [else (apply-indentation (lower-entity (parse-reals reals))
                             (for/list ([t (in-list reals)] #:when (eq? (ttype t) 'STAR)) (star-indent t)))]))

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

;; apply-indentation : entity (listof indent) -> entity   one indent per rule, in order
(define (apply-indentation e indents)
  (define (fix rules)
    (if (or (andmap zero? indents) (not (= (length rules) (length indents)))) rules (contextualize rules indents)))
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

;; contextualize : rules indents -> rules   stack entries are (list level path codes)
(define (contextualize rules indents)
  (let loop ([rs rules] [is indents] [stack '()] [acc '()])
    (cond
      [(null? rs) (reverse acc)]
      [else
       (define level (quotient (car is) 2))
       (define live (filter (lambda (s) (< (car s) level)) stack))
       (define ctx (for/first ([s (in-list live)] #:when (= (car s) (sub1 level))) s))
       (define r (if ctx (in-context (car rs) (cadr ctx) (caddr ctx)) (car rs)))
       (loop (cdr rs) (cdr is) (cons (list level (context-path r) (rule-codes r)) live) (cons r acc))])))

(define (join-path ctx p)
  (cond [(not ctx) p]
        [(or (not p) (string=? p "") (string=? p ".")) ctx]
        [(string=? ctx "") p]
        [else (string-append ctx "." p)]))
;; a rule's path as context for the rules under it ([+] -> [=])
(define (context-path r)
  (define p (rule-path* r))
  (and p (string-replace p "[+]" "[=]")))
(define (rule-path* r)
  (match r
    [(rule-card p _ _) p] [(rule-flag p _ _) p] [(rule-binding p _ _) p] [(rule-assign p _ _) p]
    [(rule-contains p _) p] [(rule-only p _) p] [(rule-obeys p _) p] [(rule-caret p _ _) p]
    [(rule-mapping p _ _ _) p] [(rule-insert p _) p] [(rule-add-element p _ _ _ _ _) p]
    [(rule-add-cr-element p _ _ _ _ _) p] [(rule-path p) p] [_ #f]))
(define (rule-codes r)
  (match r [(concept-rule cs _ _) cs] [(rule-code-caret cs _ _) cs] [(rule-code-insert cs _) cs] [_ #f]))
(define (in-context r ctx codes)
  (define (j p) (join-path ctx p))
  (define (jc cs) (if codes (append codes cs) cs))
  (match r
    [(rule-card p c f) (rule-card (j p) c f)]
    [(rule-flag p ps f) (rule-flag (j p) (map j ps) f)]
    [(rule-binding p v st) (rule-binding (j p) v st)]
    [(rule-assign p v x) (rule-assign (j p) v x)]
    [(rule-contains p items) (rule-contains (j p) items)]
    [(rule-only p ts) (rule-only (j p) ts)]
    [(rule-obeys p is) (rule-obeys (j p) is)]
    [(rule-caret p cp v) (rule-caret (j p) cp v)]
    [(rule-mapping p t c cd) (rule-mapping (j p) t c cd)]
    [(rule-insert p ref) (rule-insert (j p) ref)]
    [(rule-add-element p c f t sh d) (rule-add-element (j p) c f t sh d)]
    [(rule-add-cr-element p c f ref sh d) (rule-add-cr-element (j p) c f ref sh d)]
    [(rule-path p) (rule-path (j p))]
    [(concept-rule cs d df) (concept-rule (jc cs) d df)]
    [(rule-code-caret cs cp v) (rule-code-caret (jc cs) cp v)]
    [(rule-code-insert cs ref) (rule-code-insert (jc cs) ref)]
    [_ r]))

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
                (loc "t.fsh" 1 1 1 6)))
