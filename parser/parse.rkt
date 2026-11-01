#lang racket/base

;;; parse.rkt — the FSH document parser.
;;;
;;; A document is pre-split into entity chunks at top-level declaration keywords (so brag's
;;; single-`entity` start rule can parse each), and parameterized-RuleSet *definitions* are
;;; captured raw (their ANTLR body uses token-set negation brag can't express). The same
;;; parser backs both the embedded API and the `#lang fsh` reader.

(require brag/support
         racket/contract
         racket/match
         (only-in racket/list append*)
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt")

(provide
 (contract-out
  [parse-fsh-doc    (-> string? list?)]    ; FSH source -> list of entity ASTs
  [parse-fsh-entity (-> string? any/c)]))  ; one entity's source -> its AST

(define entity-start-types
  '(KW_ALIAS KW_PROFILE KW_EXTENSION KW_INSTANCE KW_INVARIANT KW_VALUESET
    KW_CODESYSTEM KW_RULESET KW_MAPPING KW_LOGICAL KW_RESOURCE))

(define (collect-tokens str)
  (define next (tokenize (open-input-string str)))
  (let loop ([acc '()])
    (define t (next))
    (if (or (void? t) (eof-object? t)) (reverse acc) (loop (cons t acc)))))

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

(define (parse-fsh-doc str)
  (for/list ([ch (in-list (chunk-entities (collect-tokens str)))]
             #:when (ormap real-tok? ch))
    (parse-chunk ch)))

;; parse one entity's token chunk
(define (parse-chunk toks)
  (define reals (filter real-tok? toks))
  (cond
    [(param-ruleset? reals) (capture-param-ruleset reals)]
    [else (lower-entity (parse-reals reals))]))

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
     (match-define (list 'entity inner) (g:parse-to-datum header))
     (define n (length inner))
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
