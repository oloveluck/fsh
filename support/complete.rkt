#lang racket/base

;;; support/complete.rkt — completion for an FSH document, from the text and the FHIR definitions.
;;;
;;; What is being typed decides the candidates (a Context is one of):
;;;   'keyword   the start of a line: the entity keywords and metadata keywords
;;;   'parent    after `Parent:` / `InstanceOf:`: FHIR types and the project's profiles, extensions,
;;;              logical models, resources (and instances, for InstanceOf-less contexts)
;;;   'valueset  after `from`: the project's value sets
;;;   'ruleset   after `insert`: the project's rule sets
;;;   'invariant after `obeys`: the project's invariants
;;;   'path      after `* ` (and a dotted prefix): the children of that element in the base type
;;;   'flag      after a path and a space: the flags and rule keywords
;;; A Candidate is a jsexpr CompletionItem {label kind detail? documentation?}.

(require racket/contract racket/string racket/list racket/match
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available?)
         (only-in fhir/model/registry all-types)
         (only-in fhir/lang/support entity? entity-kind entity-name entity-id)
         "symbols.rkt")
(provide
 (contract-out
  [completion-context (-> string? pair? (values symbol? string?))]
  [complete           (-> string? pair? (listof entity?) (hash/c string? string?) (listof hash?))]))

;; CompletionItemKind codes the protocol defines
(define kind-keyword 14) (define kind-class 7) (define kind-field 5) (define kind-value 12) (define kind-module 9) (define kind-reference 18)

(define entity-keywords '("Profile" "Extension" "Instance" "ValueSet" "CodeSystem" "Invariant" "RuleSet" "Mapping" "Logical" "Resource" "Alias"))
(define metadata-keywords '("Parent" "Id" "Title" "Description" "InstanceOf" "Usage" "Expression" "XPath" "Severity" "Source" "Target" "Context" "Characteristics"))
(define rule-keywords '("MS" "SU" "?!" "N" "TU" "D" "from" "only" "obeys" "contains" "insert" "and" "named" "(required)" "(extensible)" "(preferred)" "(example)" "(exactly)"))

;; completion-context : text position -> (values Context prefix)   what the cursor is in, and the
;; characters of the word being typed (what the editor will filter by)
(define (completion-context text pos)
  (define lines (regexp-split #rx"\r?\n" text))
  (define line (if (< -1 (car pos) (length lines)) (list-ref lines (car pos)) ""))
  (define before (substring line 0 (min (cdr pos) (string-length line))))
  (define (prefix rx) (let ([m (regexp-match rx before)]) (and m (cadr m))))
  (cond
    [(prefix #px"^(?:Parent|InstanceOf):\\s*(\\S*)$") => (lambda (p) (values 'parent p))]
    [(prefix #px"\\bfrom\\s+(\\S*)$") => (lambda (p) (values 'valueset p))]
    [(prefix #px"\\binsert\\s+(\\S*)$") => (lambda (p) (values 'ruleset p))]
    [(prefix #px"\\bobeys\\s+(?:\\S+\\s+and\\s+)*(\\S*)$") => (lambda (p) (values 'invariant p))]
    [(prefix #px"^\\s*\\*\\s+([A-Za-z_][A-Za-z0-9_.\\[\\]:+=$/-]*)$") => (lambda (p) (values 'path p))]
    [(regexp-match? #px"^\\s*\\*\\s+\\S+\\s+\\S*$" before) (values 'flag (or (prefix #px"(\\S*)$") ""))]
    [(regexp-match? #px"^[A-Za-z]*$" before) (values 'keyword before)]
    [else (values 'none "")]))

;; complete : text position entities aliases -> candidates
(define (complete text pos es aliases)
  (define-values (ctx prefix) (completion-context text pos))
  (define (entities-of-kind . kinds)
    (for/list ([e (in-list es)] #:when (member (entity-kind e) kinds))
      (item (entity-name e) kind-class (entity-kind e) (and (entity-id e) (format "Id: ~a" (entity-id e))))))
  (case ctx
    [(keyword) (append (for/list ([k (in-list entity-keywords)]) (item (string-append k ": ") kind-keyword "entity" #f))
                       (for/list ([k (in-list metadata-keywords)]) (item (string-append k ": ") kind-keyword "metadata" #f)))]
    [(parent) (append (entities-of-kind "Profile" "Extension" "Logical" "Resource")
                      (for/list ([k (in-list (aliases-items aliases))]) k)
                      (for/list ([t (in-list (resource-types))]) (item t kind-module "FHIR type" #f)))]
    [(valueset) (append (entities-of-kind "ValueSet") (aliases-items aliases))]
    [(ruleset) (entities-of-kind "RuleSet")]
    [(invariant) (entities-of-kind "Invariant")]
    [(path) (path-items text es aliases prefix)]
    [(flag) (for/list ([k (in-list rule-keywords)]) (item k kind-keyword "rule" #f))]
    [else '()]))

(define (item label kind detail doc)
  (define base (hasheq 'label label 'kind kind 'detail detail))
  (if doc (hash-set base 'documentation doc) base))
(define (aliases-items aliases)
  (for/list ([(k v) (in-hash aliases)]) (item k kind-reference "Alias" v)))

;; resource-types : -> (listof string)   the registered types that are resources (start upper, no dot
;; and not a backbone element: a backbone's name is its parent's plus a capitalised child)
(define (resource-types)
  (sort (for/list ([t (in-list (all-types))]
                   #:when (let ([s (symbol->string t)]) (and (char-upper-case? (string-ref s 0)) (not (backbone? s)))))
          (symbol->string t))
        string<?))
(define (backbone? s)
  ;; AccountBalance is a backbone of Account; Observation is not one of anything registered
  (for/or ([t (in-list (all-types))])
    (define p (symbol->string t))
    (and (< (string-length p) (string-length s)) (string-prefix? s p)
         (char-upper-case? (string-ref s (string-length p))) (> (string-length p) 3))))

;; path-items : text entities aliases prefix -> candidates   the base type's elements at the prefix's
;; parent path (`component.` -> the children of component); slice brackets dropped
(define (path-items text es aliases prefix)
  (define base (parent-of text es aliases))
  (define sd (and base (sd-index-available?) (resolve-sd-raw base)))
  (define els (and (hash? sd) (hash-ref (hash-ref sd 'snapshot (hasheq)) 'element '())))
  (cond
    [(not (list? els)) '()]
    [else
     (define typed (regexp-replace* #px"\\[[^\\]]*\\]" prefix ""))
     (define parent-steps (let ([parts (string-split typed "." #:trim? #f)]) (drop-right parts 1)))
     (define parent-path (string-join (cons base parent-steps) "."))
     (define depth (add1 (length parent-steps)))
     (for/list ([e (in-list els)]
                #:when (let ([p (hash-ref e 'path "")])
                         (and (string-prefix? p (string-append parent-path "."))
                              (= (length (string-split p ".")) (add1 depth))
                              (not (hash-ref e 'sliceName #f)))))
       (define name (last (string-split (hash-ref e 'path) ".")))
       (item name kind-field
             (format "~a..~a ~a" (hash-ref e 'min 0) (hash-ref e 'max "*")
                     (string-join (for/list ([t (in-list (hash-ref e 'type '()))]) (hash-ref t 'code "")) " | "))
             (hash-ref e 'short #f)))]))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  (define text "Alias: $sct = http://snomed.info/sct\n\nProfile: BP\nParent: Observation\n* component.\n* code MS\nValueSet: VS\n* $sct#1\nRuleSet: RS\n* status MS\n")
  (define es (entities-of text "/tmp/x.fsh"))
  (define aliases (aliases-of text))
  (define (ctx line col) (call-with-values (lambda () (completion-context text (cons line col))) list))
  (check-equal? (ctx 3 10) '(parent "Ob"))
  (check-equal? (ctx 4 13) '(path "component."))
  (check-equal? (ctx 5 9) '(flag "MS"))
  (check-equal? (ctx 2 3) '(keyword "Pro"))
  (check-equal? (ctx 0 0) '(keyword ""))
  (define (labels line col) (map (lambda (c) (hash-ref c 'label)) (complete text (cons line col) es aliases)))
  (check-true (and (member "Observation" (labels 3 10)) (member "BP" (labels 3 10)) #t))
  (check-false (member "ObservationComponent" (labels 3 10)))
  (check-true (and (member "code" (labels 4 13)) (member "value[x]" (labels 4 13)) #t))
  (check-false (member "status" (labels 4 13)))
  (check-true (and (member "MS" (labels 5 9)) #t))
  (check-true (and (member "Profile: " (labels 2 3)) #t))
  (check-equal? (labels 7 5) '())        ; inside a value set rule: nothing we know
  (check-equal? (map (lambda (c) (hash-ref c 'label)) (complete "Profile: X\nParent: Observation\n* code from " (cons 2 12) es aliases))
                '("VS" "$sct")))
