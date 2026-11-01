#lang s-exp syntax/module-reader

;;; reader.rkt — the #lang fhir/fsh reader.
;;;
;;; Reads the whole source as FSH, parses it into entity ASTs (the same parser the embedded API and
;;; the `raco fhir fsh` CLI use), and emits one (fsh-entity <prefab-ast>) form per entity. The prefab
;;; AST crosses the reader->expander boundary as literal data; the module language fhir/fsh/base gives
;;; the forms meaning.

fhir/fsh/base

#:read        fsh-read
#:read-syntax fsh-read-syntax
#:whole-body-readers? #t

(require (only-in fhir/fsh/parser/parse parse-fsh-doc)
         (only-in racket/port port->string))

(define (fsh-read-syntax src in)
  (for/list ([e (in-list (parse-fsh-doc (port->string in)))])
    (datum->syntax #f (list 'fsh-entity e))))

(define (fsh-read in)
  (map syntax->datum (fsh-read-syntax #f in)))
