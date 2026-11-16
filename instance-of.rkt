#lang racket/base

;;; instance-of.rkt — a resource written as an Instance OF the profile it claims.
;;;
;;; `fhir->fsh` writes a resource rule by rule: `InstanceOf: Observation`, its profile as a
;;; `meta.profile` rule, every list entry by its index, every value the resource holds. FSH as it is
;;; authored says less: `InstanceOf: TumorSize`, entries by the profile's slice names, and nothing the
;;; profile fixes — the compiler writes those. `fhir->fsh/of-profile` answers that text.
;;;
;;; It asks the COMPILER what it would write, and never guesses: each shortening (the profile as
;;; InstanceOf, an entry named by a slice, a lone `[0]` dropped, a rule left out) is kept only when
;;; the text still compiles, in the context the caller set (`current-sd-packages`,
;;; `fsh-fhir-version`, the registry), to exactly the resource. So the law holds by construction:
;;;     the first resource of that id that (fsh->fhir (fhir->fsh/of-profile r)) gives = r
;;; A resource that claims no single profile, or whose profile nothing in context defines, comes back
;;; as `fhir->fsh` writes it.

(require racket/list racket/string racket/contract
         (only-in "decompile.rkt" fhir->fsh)
         (only-in "compile.rkt" fsh->fhir)
         (only-in "instance.rkt" registered-sd)
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available?))

(provide
 (contract-out
  ;; fhir->fsh/of-profile : resource -> fsh-text
  [fhir->fsh/of-profile (-> hash? string?)]))

;; the rules past which leaving rules out one by one is not tried (a compile per rule: a Bundle of
;; a thousand rules would take minutes; its InstanceOf and slices are still written)
(define rule-limit 150)

;; comparable : jsexpr -> jsexpr   numbers as exact: 1.2 read from JSON is the 1.2 a rule compiles to
(define (comparable j)
  (cond [(hash? j) (for/hasheq ([(k v) (in-hash j)]) (values k (comparable v)))]
        [(list? j) (map comparable j)]
        [(real? j) (inexact->exact j)]
        [else j]))

;; profile-definition : canonical -> StructureDefinition | #f   from the packages in context, else registered
(define (profile-definition url)
  (define u (car (string-split url "|")))
  (or (and (sd-index-available?) (resolve-sd-raw u)) (registered-sd u)))

;; slices-of : StructureDefinition -> (listof (cons path slice))   the slices it names directly on
;; an element of the resource (`component` . `tumorLongestDimension`): not slices inside a slice,
;; nor type slices of a choice
(define (slices-of sd)
  (define elements (hash-ref (or (hash-ref sd 'snapshot #f) (hash-ref sd 'differential (hasheq))) 'element '()))
  (remove-duplicates
   (for/list ([el (in-list elements)]
              #:when (and (string? (hash-ref el 'sliceName #f))
                          (not (regexp-match? #rx":.*[.:]|\\[x\\]" (hash-ref el 'id "")))))
     (cons (string-join (cdr (string-split (hash-ref el 'path) ".")) ".") (hash-ref el 'sliceName)))))

;; rule? : line -> boolean   a line that may be left out: a rule, or the Usage the compiler assumes
(define (rule? l) (regexp-match? #rx"^([*] |Usage:)" l))

(define (fhir->fsh/of-profile j)
  (define text (fhir->fsh j))
  (define wanted (comparable j))
  (define id (hash-ref j 'id #f))
  ;; says-it? : lines -> boolean   the text compiles to exactly the resource
  (define (says-it? ls)
    (define rs (with-handlers ([exn:fail? (lambda (_) '())]) (fsh->fhir (string-join ls "\n"))))
    (for/or ([r (in-list rs)]) (and (equal? (hash-ref r 'id #f) id) (equal? (comparable r) wanted))))
  ;; keep-if : lines lines -> lines   the new lines when they differ and still say the resource
  (define (keep-if new old) (if (and (not (equal? new old)) (says-it? new)) new old))
  (define (rewrite ls from to) (for/list ([l (in-list ls)]) (if (rule? l) (string-replace l from to) l)))
  (define profiles (let ([m (hash-ref j 'meta #f)]) (if (hash? m) (hash-ref m 'profile '()) '())))
  (define sd (and (list? profiles) (= (length profiles) 1) (string? (car profiles)) (profile-definition (car profiles))))
  (define lines (string-split text "\n"))
  ;; as an instance of the profile, named as briefly as the compiler finds it: name, id, canonical
  (define started
    (and sd id
         (let ([body (filter (lambda (l) (not (regexp-match? #rx"^[*] meta[.]profile(\\[0\\])? = " l))) lines)])
           (for/or ([name (in-list (remove-duplicates (filter string? (list (hash-ref sd 'name #f) (hash-ref sd 'id #f) (hash-ref sd 'url #f)))))])
             (define ls (for/list ([l (in-list body)]) (if (regexp-match? #rx"^InstanceOf:" l) (string-append "InstanceOf: " name) l)))
             (and (says-it? ls) ls)))))
  (cond
    [(not started) text]
    [else
     ;; entries by slice: `component[0]` as `component[<slice>]`, the first slice that says the same
     (define sliced
       (for/fold ([ls started]) ([ps (in-list (slices-of sd))])
         (define indexes
           (remove-duplicates
            (for*/list ([l (in-list ls)]
                        [m (in-value (regexp-match (pregexp (string-append "^[*] " (regexp-quote (car ps)) "\\[([0-9]+)\\]")) l))]
                        #:when m)
              (cadr m))))
         (for/fold ([ls ls]) ([i (in-list indexes)])
           (keep-if (rewrite ls (format "* ~a[~a]" (car ps) i) (format "* ~a[~a]" (car ps) (cdr ps))) ls))))
     ;; a lone first entry needs no index
     (define unindexed
       (for/fold ([ls sliced]) ([m (in-list (remove-duplicates (append-map (lambda (l) (if (rule? l) (regexp-match* #px"[A-Za-z][A-Za-z0-9.]*\\[0\\]" l) '())) sliced)))])
         (keep-if (rewrite ls m (substring m 0 (- (string-length m) 3))) ls)))
     ;; what the compiler writes by itself is not said
     (define own
       (if (> (count rule? unindexed) rule-limit)
           unindexed
           (for/fold ([kept unindexed]) ([l (in-list unindexed)] #:when (rule? l))
             (keep-if (remove l kept) kept))))
     (string-join own "\n")]))

(module+ test
  (require rackunit (only-in fhir/r4/schemas) (only-in "compile.rkt" fsh-fhir-version)
           (only-in fhir/registry register-resources! unregister-source!))
  ;; a profile registered as a required `#lang fsh` module would register it
  (define profile
    (car (parameterize ([fsh-fhir-version "4.0.1"])
           (fsh->fhir (string-append
                       "Profile: Pulse\nParent: Observation\nId: pulse\n"
                       "* code = http://loinc.org#8867-4\n"
                       "* component ^slicing.discriminator.type = #pattern\n"
                       "* component ^slicing.discriminator.path = \"code\"\n"
                       "* component ^slicing.rules = #open\n"
                       "* component contains rest 0..1\n"
                       "* component[rest].code = http://loinc.org#40443-4\n")))))
  (register-resources! (list profile) #:source 'instance-of-test)
  (define obs
    (hasheq 'resourceType "Observation" 'id "p1" 'status "final"
            'meta (hasheq 'profile (list (hash-ref profile 'url)))
            'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "8867-4")))
            'subject (hasheq 'reference "Patient/a")
            'valueQuantity (hasheq 'value 72 'unit "/min" 'system "http://unitsofmeasure.org" 'code "/min")
            'component (list (hasheq 'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "40443-4")))
                                     'valueQuantity (hasheq 'value 61 'unit "/min" 'system "http://unitsofmeasure.org" 'code "/min")))))
  (parameterize ([fsh-fhir-version "4.0.1"])
    (define text (fhir->fsh/of-profile obs))
    ;; an instance of its profile, the entry by its slice, the codes the profile fixes not said
    (check-regexp-match #rx"InstanceOf: Pulse" text)
    (check-regexp-match #rx"[*] component\\[rest\\][.]valueQuantity = 61 '/min'" text)
    (check-false (regexp-match? #rx"8867-4|40443-4|meta[.]profile|Usage" text) text)
    ;; and it is the resource
    (check-equal? (comparable (findf (lambda (r) (equal? (hash-ref r 'id #f) "p1")) (fsh->fhir text))) (comparable obs))
    ;; a resource that claims no profile, or one nothing defines, is written as fhir->fsh writes it
    (define plain (hash-remove obs 'meta))
    (check-equal? (fhir->fsh/of-profile plain) (fhir->fsh plain))
    (define elsewhere (hash-set obs 'meta (hasheq 'profile (list "http://example.org/elsewhere/StructureDefinition/x"))))
    (check-equal? (fhir->fsh/of-profile elsewhere) (fhir->fsh elsewhere)))
  (unregister-source! 'instance-of-test))
