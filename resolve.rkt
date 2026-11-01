#lang racket/base

;;; resolve.rkt — resolve an FSH element path against the R5 type tower.
;;;
;;; The FSH compiler needs two things from the information model: the FHIR TYPE CODE of an element
;;; (to pick fixed<Type> vs pattern<Type> on a profile, and to coerce Instance values), and a field's
;;; cardinality KIND (list vs single, to nest Instance assignment rules correctly). Both are read from
;;; the compile-time registry via `model/introspect`'s `full-field-specs` (inherited ⊕ own), walking a
;;; dotted path step-by-step. Everything is lenient: an unresolvable path returns #f / 'optional rather
;;; than failing the whole compile.

(require racket/contract
         racket/string
         (only-in fhir/model/introspect full-field-specs)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/model/schema
                  fs-name fs-ftype fs-kind ftype-tag ftype-prim-code ftype-type-name))

(provide
 (contract-out
  ;; root-type, dotted rel-path (e.g. 'Patient "name.family") -> the element's FHIR type code, or #f
  [element-fhir-type (-> symbol? string? (or/c string? #f))]
  ;; a single field's (kind ftype) within a parent type (for Instance nesting); lenient fallback
  [field-info        (-> symbol? symbol? (values (or/c 'single 'optional 'list) vector?))]))

;; find a field-spec by name within a type's full schema
(define (find-field type fname)
  (for/or ([fs (in-list (full-field-specs type))] #:when (eq? (fs-name fs) fname)) fs))

(define (field-info type fname)
  (define fs (and (type-registered? type) (find-field type fname)))
  (if fs (values (fs-kind fs) (fs-ftype fs)) (values 'optional (vector 'unknown))))

;; the type code of a dotted path's final step, walking complex types via the registry
(define (element-fhir-type root rel-path)
  (let loop ([type root] [steps (string-split rel-path ".")])
    (cond
      [(null? steps) #f]
      [(not (type-registered? type)) #f]
      [else
       (define fs (find-field type (string->symbol (strip-brackets (car steps)))))
       (cond
         [(not fs) #f]
         [(null? (cdr steps)) (ftype->code (fs-ftype fs))]
         [else
          (define ft (fs-ftype fs))
          (if (eq? (ftype-tag ft) 'type) (loop (ftype-type-name ft) (cdr steps)) #f)])])))

;; a path step may carry [slice]/[x] brackets; the element name is the part before "["
(define (strip-brackets s)
  (let ([i (for/or ([c (in-string s)] [j (in-naturals)]) (and (char=? c #\[) j))])
    (if i (substring s 0 i) s)))

(define (ftype->code ft)
  (case (ftype-tag ft)
    [(prim) (ftype-prim-code ft)]
    [(type) (symbol->string (ftype-type-name ft))]
    [else #f]))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  (check-equal? (element-fhir-type 'Patient "gender") "code")
  (check-equal? (element-fhir-type 'Patient "maritalStatus") "CodeableConcept")
  (check-equal? (element-fhir-type 'Patient "name.family") "string")
  (check-equal? (element-fhir-type 'Patient "bogus") #f)
  (define-values (k _ft) (field-info 'Patient 'name))
  (check-eq? k 'list))
