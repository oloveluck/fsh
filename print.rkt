#lang racket/base

;;; print.rkt — render FSH AST back to FSH text (the inverse of parse.rkt).
;;;
;;; Used by the FHIR->FSH decompiler and verified by a parse∘print∘parse fixpoint test.
;;; Paths and caret paths are stored as raw strings in the rule structs, so they print
;;; directly; values reconstruct their FSH literal form.

(require racket/match
         racket/string
         racket/contract
         "parser/ast.rkt")

(provide
 (contract-out
  [print-doc    (-> list? string?)]     ; entities -> an FSH document string
  [print-entity (-> any/c string?)]     ; one FSH entity AST -> its FSH text
  [print-value  (-> any/c string?)]))   ; an FSH value -> its FSH text

(define (print-doc entities) (string-join (map print-entity entities) "\n\n"))

;; -------------------------------------------------------------------
;; entities
;; -------------------------------------------------------------------
(define (print-entity e)
  (match e
    [(fsh-alias name v) (format "Alias: ~a = ~a" name v)]
    [(fsh-profile name meta rules)   (entity "Profile" name meta rules)]
    [(fsh-extension name meta rules) (entity "Extension" name meta rules)]
    [(fsh-logical name meta rules)   (entity "Logical" name meta rules)]
    [(fsh-resource name meta rules)  (entity "Resource" name meta rules)]
    [(fsh-instance name meta rules)  (entity "Instance" name meta rules)]
    [(fsh-invariant name meta rules) (entity "Invariant" name meta rules)]
    [(fsh-valueset name meta rules)  (entity "ValueSet" name meta rules)]
    [(fsh-codesystem name meta rules)(entity "CodeSystem" name meta rules)]
    [(fsh-mapping name meta rules)   (entity "Mapping" name meta rules)]
    [(fsh-ruleset name rules)
     (lines (cons (format "RuleSet: ~a" name) (map print-rule rules)))]
    [(fsh-param-ruleset name params _body)
     (format "RuleSet: ~a(~a)" name (string-join params ", "))]
    [_ (format ";; <unprintable ~a>" e)]))

(define (entity kw name meta rules)
  (lines (append (list (format "~a: ~a" kw name))
                 (map print-meta meta)
                 (map print-rule rules))))

(define (lines xs) (string-join (filter values xs) "\n"))

;; -------------------------------------------------------------------
;; metadata
;; -------------------------------------------------------------------
(define (print-meta kv)
  (define key (car kv))
  (define v (cdr kv))
  (case key
    [(parent)      (format "Parent: ~a" v)]
    [(id)          (format "Id: ~a" v)]
    [(instanceOf)  (format "InstanceOf: ~a" v)]
    [(source)      (format "Source: ~a" v)]
    [(usage)       (format "Usage: ~a" v)]
    [(severity)    (format "Severity: ~a" v)]
    [(title)       (format "Title: ~a" (qstr v))]
    [(description) (format "Description: ~a" (qstr v))]
    [(expression)  (format "Expression: ~a" (qstr v))]
    [(xpath)       (format "XPath: ~a" (qstr v))]
    [(target)      (format "Target: ~a" (qstr v))]
    [(characteristics) (format "Characteristics: ~a" (string-join v ", "))]
    [(context)     (format "Context: ~a" (print-context v))]
    [else          (format ";; ~a: ~a" key v)]))

(define (print-context items)
  (string-join (for/list ([x (in-list items)])
                 (match x [(list 'name n) n] [(? string? s) s] [_ (format "~a" x)])) ", "))

;; -------------------------------------------------------------------
;; rules
;; -------------------------------------------------------------------
(define (print-rule r)
  (match r
    [(rule-card path card flags) (star (sp path card (flags->str flags)))]
    [(rule-flag path extra flags)
     (star (sp (string-join (cons path (map (lambda (p) (string-append "and " p)) extra)) " ")
               (flags->str flags)))]
    [(rule-binding path vs strength)
     (star (sp path "from" vs (and strength (format "(~a)" strength))))]
    [(rule-assign path v exact?)
     (star (sp path "=" (print-value v) (and exact? "(exactly)")))]
    [(rule-contains path items)
     (star (sp path "contains" (string-join (map print-item items) " and ")))]
    [(rule-only path types) (star (sp path "only" (string-join types " or ")))]
    [(rule-obeys path invs) (star (sp path "obeys" (string-join invs " and ")))]
    [(rule-caret path cp v) (star (sp path cp "=" (print-value v)))]
    [(rule-code-caret codes cp v) (star (sp (string-join codes " ") cp "=" (print-value v)))]
    [(rule-mapping path target comment code)
     (star (sp path "->" (qstr target) (and comment (qstr comment)) code))]
    [(rule-insert path ref) (star (sp path "insert" ref))]
    [(rule-code-insert codes ref) (star (sp (string-join codes " ") "insert" ref))]
    [(rule-add-element path card flags types short def)
     (star (sp path card (flags->str flags) (string-join types " or ")
               (and short (qstr short)) (and def (qstr def))))]
    [(rule-add-cr-element path card flags ref short def)
     (star (sp path card (flags->str flags) "contentReference" ref (qstr short) (and def (qstr def))))]
    [(rule-path path) (star path)]
    [(concept-rule codes display def)
     (star (sp (string-join codes " ") (and display (qstr display)) (and def (qstr def))))]
    [(vs-component include? body)
     (star (sp (and (not include?) "exclude") (and include? "include") (print-vs-body body)))]
    [_ (format ";; <unprintable rule ~a>" r)]))

(define (star s) (string-append "* " s))
;; join non-#f, non-empty parts with single spaces (an empty path segment is a root element)
(define (sp . parts)
  (string-join (filter (lambda (p) (and p (not (equal? p ""))))
                       (map (lambda (p) (and p (format "~a" p))) parts)) " "))
(define (flags->str flags) (and (pair? flags) (string-join (map symbol->string flags) " ")))

(define (print-item it)
  (match it
    [(contains-item name named card flags)
     (sp name (and named (format "named ~a" named)) card (flags->str flags))]))

;; -------------------------------------------------------------------
;; ValueSet component bodies (raw datum from the grammar)
;; -------------------------------------------------------------------
(define (print-vs-body body)
  (string-join (filter values (map print-vs-node body)) " "))

(define (print-vs-node n)
  (match n
    [(list 'vsConceptComponent inner ...) (string-join (map print-vs-node inner) " ")]
    [(list 'vsFilterComponent "codes" rest ...) (sp "codes" (string-join (map print-vs-node rest) " "))]
    [(list 'vsComponentFrom "from" rest ...) (sp "from" (string-join (map print-vs-node rest) " "))]
    [(list 'vsFromSystem "system" (list 'name s)) (format "system ~a" s)]
    [(list 'vsFromValueset "valueset" rest ...)
     (sp "valueset" (string-join (for/list ([x rest] #:when (and (pair? x) (eq? (car x) 'name))) (cadr x)) " and "))]
    [(list 'code c d ...) (sp (code-text* c) (and (pair? d) (qstr (car d))))]
    [(list 'name s) s]
    [(? string? s) s]
    [_ #f]))

;; -------------------------------------------------------------------
;; values
;; -------------------------------------------------------------------
(define (print-value v)
  (match v
    [(? string? s) (qstr s)]
    [(? boolean? b) (if b "true" "false")]
    [(? number? n) (num->str n)]
    [(fsh-code c d) (sp c (and d (qstr d)))]
    [(fsh-quantity val u d) (sp (and val (num->str val)) (format "'~a'" u) (and d (qstr d)))]
    [(fsh-reference t d) (sp t (and d (qstr d)))]
    [(fsh-canonical t) t]
    [(fsh-ratio a b) (format "~a : ~a" (print-value a) (print-value b))]
    [_ (format "~a" v)]))

(define (num->str n)
  (if (and (inexact? n) (integer? n)) (number->string n) (number->string n)))

(define (qstr s)
  (string-append "\"" (regexp-replace* #rx"\"" (regexp-replace* #rx"\\\\" s "\\\\\\\\") "\\\\\"") "\""))

(define (code-text* x) (if (code-tok? x) (code-tok-text x) x))
