#lang racket/base

;;; base.rkt — the module language for #lang fsh.
;;;
;;; The reader (lang/reader.rkt) parses an FSH source file into entity ASTs and emits one
;;; (fsh-entity <prefab-ast>) form per entity, pointing them here. This custom #%module-begin
;;; collects the ASTs, compiles them to FHIR jsexprs (compile.rkt), and ALSO realises each as a typed
;;; `fhir` node via from-json (the repo's canonical value). Run the file to print the FHIR JSON.

(require (for-syntax racket/base syntax/parse)
         json
         (only-in fhir/r5/schemas)                     ; register the R5 tower (compile + from-json)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/registry register-resources!)
         "compile.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [fsh-module-begin #%module-begin])
         fsh-entity)

;; (fsh-entity ast) yields the prefab entity-AST literal; module-begin collects them.
(define-syntax-rule (fsh-entity ast) (quote ast))

;; the module body the reader produces: (fsh-entity <entity>) forms, each an FSH entity AST (a
;; prefab whose key starts with fsh-) — anything else is a precise error here
(begin-for-syntax
  (define-syntax-class entity-form
    #:description "an (fsh-entity <entity>) form, as the #lang fsh reader writes it"
    (pattern ((~datum fsh-entity) ast)
             #:fail-unless (let ([k (prefab-struct-key (syntax->datum #'ast))])
                             (and (symbol? k) (regexp-match? #rx"^fsh-" (symbol->string k))))
             "not an FSH entity AST")))

(define-syntax (fsh-module-begin stx)
  (syntax-parse stx
    [(_ form:entity-form ...)
     #'(#%module-begin
        (define document (list form ...))
        (define compiled (compile-doc document))          ; (listof FHIR jsexpr)
        ;; the module system as the FHIR registry: what this file defines resolves by canonical
        ;; (fhir/registry) — a profile for `#:profile`, a value set for `$expand`, a logical model
        ;; for a map's `uses` — once the module is required
        (register-resources! compiled #:source (variable-reference->module-source (#%variable-reference)))
        (define resources                                 ; (listof typed fhir node | jsexpr)
          (for/list ([j (in-list compiled)])
            (let ([r (from-json j #:lenient? #t)]) (if (ok? r) (ok-value r) j))))
        (provide document compiled resources)
        (module+ main
          (for ([j (in-list compiled)]) (displayln (jsexpr->string j)))))]))
