#lang racket/base

;;; parse.rkt — the FSH document parser.
;;;
;;; A document is pre-split into entity chunks at top-level declaration keywords (so brag's
;;; single-`entity` start rule can parse each), and parameterized-RuleSet *definitions* are
;;; captured raw (their ANTLR body uses token-set negation brag can't express). The same
;;; parser backs both the embedded API and the `#lang fsh` reader.

(require brag/support
         racket/contract
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
    [else (lower-entity (g:parse-to-datum reals))]))

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
