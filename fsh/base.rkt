#lang racket/base

;;; base.rkt — the module language for #lang fhir/fsh.
;;;
;;; The reader (lang/reader.rkt) parses an FSH source file into entity ASTs and emits one
;;; (fsh-entity <prefab-ast>) form per entity, pointing them here. This custom #%module-begin
;;; collects the ASTs, compiles them to FHIR jsexprs (compile.rkt), and ALSO realises each as a typed
;;; `fhir` node via from-json (the repo's canonical value). Run the file to print the FHIR JSON.

(require (for-syntax racket/base)
         json
         (only-in fhir/r5/schemas)                     ; register the R5 tower (compile + from-json)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         "compile.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [fsh-module-begin #%module-begin])
         fsh-entity)

;; (fsh-entity ast) yields the prefab entity-AST literal; module-begin collects them.
(define-syntax-rule (fsh-entity ast) (quote ast))

(define-syntax (fsh-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin
        (define document (list form ...))
        (define compiled (compile-doc document))          ; (listof FHIR jsexpr)
        (define resources                                 ; (listof typed fhir node | jsexpr)
          (for/list ([j (in-list compiled)])
            (let ([r (from-json j #:lenient? #t)]) (if (ok? r) (ok-value r) j))))
        (provide document compiled resources)
        (module+ main
          (for ([j (in-list compiled)]) (displayln (jsexpr->string j)))))]))
