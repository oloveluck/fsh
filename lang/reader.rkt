#lang s-exp syntax/module-reader

;;; reader.rkt — the #lang fsh reader.
;;;
;;; Reads the whole source as FSH, parses it into entity ASTs (the same parser the embedded API and
;;; the `raco fhir fsh` CLI use), and emits one (fsh-entity <prefab-ast>) form per entity. The prefab
;;; AST crosses the reader->expander boundary as literal data; the module language fsh/base gives
;;; the forms meaning.

fsh/base

#:read        fsh-read
#:read-syntax fsh-read-syntax
#:whole-body-readers? #t

(require (only-in fsh/parser/parse parse-fsh-doc/located)
         (only-in fsh/compile compile-doc/diagnostics)
         (only-in fsh/base with-config)
         (only-in fhir/diagnostic raise-diagnostic diagnostic-severity)
         (only-in racket/port port->string))

(define (source-name src) (cond [(path? src) (path->string src)] [(string? src) src] [else #f]))
;; a parse error raises exn:fail:diagnostic, positioned in this file (fhir/diagnostic); so does a
;; rule the compiler finds wrong on its own (`* birthdate 1..1`: `birthdate` is not an element of
;; Patient), as `raco make` fails on `#lang fml`'s and `#lang cql`'s — it used to build, and every
;; valid Patient then failed validation against a constraint on an element nothing has
;; each form carries its entity's position (a prefab `loc`), so an entity that fails when the module
;; runs (an InstanceOf another file defines) is reported at its header, not with a stack trace
(define (fsh-read-syntax src in)
  (define located (parse-fsh-doc/located (port->string in) #:source (source-name src)))
  (check! src (map car located))
  (for/list ([p (in-list located)])
    (datum->syntax #f (list 'fsh-entity (car p) (cdr p)))))

;; check! : source entities -> void   the first rule-level error, raised at its rule. Only the rule
;; checks: what another file defines (a Parent, a RuleSet) is not this file's mistake
(define (check! src entities)
  (define rule-ds
    (with-handlers ([exn:fail? (lambda (_) '())])            ; a compile that cannot run decides nothing
      (with-config src (lambda () (let-values ([(_out _errors ds) (compile-doc/diagnostics entities)]) ds)))))
  (define first-error (for/first ([d (in-list rule-ds)] #:when (eq? (diagnostic-severity d) 'error)) d))
  (when first-error (raise-diagnostic first-error)))

(define (fsh-read in)
  (map syntax->datum (fsh-read-syntax #f in)))
