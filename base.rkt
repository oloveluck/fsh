#lang racket/base

;;; base.rkt — the module language for #lang fsh.
;;;
;;; The reader (lang/reader.rkt) parses an FSH source file into entity ASTs and emits one
;;; (fsh-entity <prefab-ast>) form per entity, pointing them here. This custom #%module-begin
;;; collects the ASTs, compiles them to FHIR jsexprs (compile.rkt), and ALSO realises each as a typed
;;; `fhir` node via from-json (the repo's canonical value). Run the file to print the FHIR JSON.
;;; Like `raco fsh`, it compiles with the nearest sushi-config.yaml (beside the file, or one or two
;;; levels up): its canonical, FHIR version and dependency packages — so the module and the command
;;; line give a profile the same url. `(canonical-of "IntakePatient")` is that url, by name or id.

(require (for-syntax racket/base syntax/parse)
         json
         (only-in fhir/r5/schemas)                     ; register the R5 tower (compile + from-json)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/registry register-resources!)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in "project.rkt" find-config)
         (only-in "ig-config.rkt" read-ig-config ig-config-canonical ig-config-fhir-version ig-config-dependencies dependency-packages)
         "compile.rkt")

;; with-config : module-source thunk -> value   the thunk under the project's sushi-config.yaml
(define (with-config src thunk)
  (define dir (and (path? src) (let-values ([(d _n _x) (split-path src)]) (and (path? d) d))))
  (define cfg (and dir (let ([f (find-config dir)]) (and f (read-ig-config f)))))
  (cond
    [(not cfg) (thunk)]
    [else
     (define-values (deps _missing) (dependency-packages (or (ig-config-dependencies cfg) '())))
     (parameterize ([canonical-base (or (ig-config-canonical cfg) (canonical-base))]
                    [fsh-fhir-version (or (ig-config-fhir-version cfg) (fsh-fhir-version))]
                    [current-sd-packages (append deps (current-sd-packages))])
       (thunk))]))
;; canonicals : compiled -> (hash name|id -> url)
(define (canonicals compiled)
  (for*/fold ([h (hash)]) ([j (in-list compiled)] #:when (string? (hash-ref j 'url #f))
                          [k (in-list (list (hash-ref j 'name #f) (hash-ref j 'id #f)))] #:when (string? k))
    (hash-set h k (hash-ref j 'url))))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [fsh-module-begin #%module-begin])
         fsh-entity
         with-config)

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
        (define compiled                                  ; (listof FHIR jsexpr)
          (with-config (variable-reference->module-source (#%variable-reference)) (lambda () (compile-doc document))))
        ;; canonical-of : name|id -> url   a profile's, extension's, value set's or code system's
        (define (canonical-of name)
          (hash-ref (canonicals compiled) name
                    (lambda () (raise-arguments-error 'canonical-of "this file defines nothing of that name"
                                                      "name" name "defined" (sort (hash-keys (canonicals compiled)) string<?)))))
        ;; the module system as the FHIR registry: what this file defines resolves by canonical
        ;; (fhir/registry) — a profile for `#:profile`, a value set for `$expand`, a logical model
        ;; for a map's `uses` — once the module is required
        (register-resources! compiled #:source (variable-reference->module-source (#%variable-reference)))
        (define resources                                 ; (listof typed fhir node | jsexpr)
          (for/list ([j (in-list compiled)])
            (let ([r (from-json j #:lenient? #t)]) (if (ok? r) (ok-value r) j))))
        (provide document compiled resources canonical-of)
        (module+ main
          (for ([j (in-list compiled)]) (displayln (jsexpr->string j)))))]))
