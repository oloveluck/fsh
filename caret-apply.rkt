#lang racket/base
;;; caret-apply.rkt — caret rules (`^path = value`) applied to a built resource, typed and schema-aware.
(require racket/match
         "parser/ast.rkt"
         "caret.rkt"
         (only-in fhir/model/registry type-registered?)
         "context.rkt"
         "instance.rkt")
(provide apply-carets typed-caret-set)
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
  (define cp1 (regexp-replace* #px"\\[(\\$[^\\]]+)\\]" cp0
                               (lambda (all a) (string-append "[" (resolve-alias a (current-aliases)) "]"))))
  ;; `^extension[FMM]`: the resource's (or element's) own extension may be named by the Extension's
  ;; name, id or alias, which SUSHI resolves to its canonical; a nested `.extension[code]` stays a
  ;; relative url
  (define cp (match (regexp-match #px"^(\\^?)(extension|modifierExtension)\\[([^\\]]+)\\](.*)$" cp1)
               [(list _ caret field name rest)
                #:when (not (regexp-match? #px"^[0-9+=]+$|[:/]" name))
                (define url (extension-url-of name))
                (if url (string-append caret field "[" url "]" rest) cp1)]
               [_ cp1]))
  (if (and type (type-registered? type))
      (caret-set js cp v #:type type #:counters counters
                 #:coerce (lambda (val ftype) (coerce (resolve val (current-aliases)) ftype)))
      (caret-set js cp (caret-val v))))
(define (caret-val v)
  (match v
    [(struct fsh-code (c _)) (strip# c)]
    [(? string? s) s] [(? boolean? b) b] [(? number? n) n]
    [_ (format "~a" v)]))
