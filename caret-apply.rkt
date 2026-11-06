#lang racket/base
;;; caret-apply.rkt — caret rules (`^path = value`) applied to a built resource, typed and schema-aware; the
;;; base-snapshot order of a differential; the mappings an entity inherits.
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
         "instance.rkt")
(provide apply-carets inherited-mappings snapshot-order typed-caret-set current-slice-declarations)
;; current-slice-declarations : (parameter (or #f (hash (cons path slice) -> index)))   the order in
;; which the rules' `contains` DECLARE each slice of a path; the differential follows it (SUSHI's
;; order), not the first rule that happens to mention a slice
(define current-slice-declarations (make-parameter #f))
;; ---- caret application (SD/VS/CS root-level, path #f) ---------------------------------------
;; root-level carets (`* ^status = #active`), set by the RESOURCE's schema (caret.rkt typed mode):
;; repeating elements, soft indices, extension[url], and values shaped for their target type
(define (apply-carets js rules)
  (define type (and (hash? js) (let ([t (hash-ref js 'resourceType #f)]) (and t (string->symbol t)))))
  (define counters (make-hash))
  (for/fold ([js js]) ([r (in-list rules)])
    (match r
      [(rule-caret #f cp v) (typed-caret-set js cp v type counters)]
      [(rule-code-caret '() cp v) (typed-caret-set js cp v type counters)]
      [_ js])))
;; typed-caret-set : jsexpr caret-path fsh-value type counters -> jsexpr
(define (typed-caret-set js cp0 v type counters)
  ;; `^extension[$alias]`: the url in a bracket may be an alias
  (define cp (regexp-replace* #px"\\[(\\$[^\\]]+)\\]" cp0
                              (lambda (all a) (string-append "[" (resolve-alias a (current-aliases)) "]"))))
  (if (and type (type-registered? type))
      (caret-set js cp v #:type type #:counters counters
                 #:coerce (lambda (val ftype) (coerce (resolve val (current-aliases)) ftype)))
      (caret-set js cp (caret-val v))))
;; snapshot-order : base-type (listof element) -> same elements, in the base definition's order.
;; SUSHI emits a differential in snapshot order, not rule order. Elements are compared segment by
;; segment on their ids (`Observation.component:systolic.code`): first the position of the path
;; prefix in the base snapshot (unknown paths — inside a datatype — sort after known ones), then the
;; slice (the unsliced element before its slices, slices in first-appearance order); ties keep rule
;; order. Without a core package to read (no SD index) the rule order is kept.
(define (snapshot-order base-type elements)
  (define base (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define snap (and (hash? base) (let ([sn (hash-ref base 'snapshot #f)]) (and (hash? sn) (hash-ref sn 'element #f)))))
  (cond
    [(not (list? snap)) elements]
    [else
     (define pos (for/hash ([e (in-list snap)] [i (in-naturals)] #:when (hash-ref e 'path #f))
                   (values (hash-ref e 'path) i)))
     ;; a nested extension's children (`Extension.extension.url`) are not in the base snapshot:
     ;; they take the Extension datatype's order (url before value[x])
     (define (position path)
       (hash-ref pos (regexp-replace #rx"\\[x\\]$" path "[x]")
                 (lambda () (hash-ref pos path
                                      (lambda () (match (regexp-match #px"[.](?:modifierE|e)xtension[.]([^.]+)$" path)
                                                   [(list _ last) (+ 500000 (hash-ref pos (string-append "Extension." last) 1000000))]
                                                   [_ 1000000]))))))
     ;; slices rank by their `contains` declaration order (current-slice-declarations), then by
     ;; first appearance
     (define declared (or (current-slice-declarations) (hash)))
     (define slice-rank (make-hash))                       ; (path-prefix . slice) -> first appearance
     (define (segments id)                                  ; "A.b:s.c" -> '(("A" #f) ("b" "s") ("c" #f))
       (for/list ([seg (in-list (string-split id "."))])
         (match (regexp-match #px"^([^:]+)(?::(.+))?$" seg) [(list _ n sl) (list n sl)] [#f (list seg #f)])))
     (define (key id)
       (let loop ([segs (segments id)] [prefix ""] [acc '()])
         (cond
           [(null? segs) (reverse acc)]
           [else
            (match-define (list n sl) (car segs))
            (define path (if (string=? prefix "") n (string-append prefix "." n)))
            (define p (position path))
            (define r (if sl
                          (hash-ref declared (cons path sl)
                                    (lambda () (+ 100000 (hash-ref! slice-rank (cons path sl) (lambda () (hash-count slice-rank))))))
                          -1))
            (loop (cdr segs) path (list* r p acc))])))
     (define keyed (for/list ([e (in-list elements)] [i (in-naturals)]) (list (key (hash-ref e 'id "")) i e)))
     (map caddr (sort keyed (lambda (a b) (key<? (car a) (car b) (cadr a) (cadr b)))))]))
;; lexicographic on integer lists; a prefix sorts first; ties fall back to rule order
(define (key<? a b ia ib)
  (cond [(and (null? a) (null? b)) (< ia ib)]
        [(null? a) #t] [(null? b) #f]
        [(< (car a) (car b)) #t] [(> (car a) (car b)) #f]
        [else (key<? (cdr a) (cdr b) ia ib)]))
;; inherited-mappings : base type -> the core SD's `mapping` list, or #f when no core package is
;; available to the SD index (fail open: the profile then simply carries none)
(define (inherited-mappings base-type)
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define m (and (hash? sd) (hash-ref sd 'mapping #f)))
  (and (list? m) (pair? m) m))
(define (caret-val v)
  (match v
    [(struct fsh-code (c _)) (strip# c)]
    [(? string? s) s] [(? boolean? b) b] [(? number? n) n]
    [_ (format "~a" v)]))
