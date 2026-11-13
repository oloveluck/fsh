#lang racket/base

;;; pack.rkt — an FSH project's compiled resources as a FHIR NPM package (.tgz).
;;;
;;; The package is what the rest of the toolchain consumes (`raco fhir-server serve-ig <pkg.tgz>`, an SD
;;; index, a validator): package/package.json from the project's sushi-config.yaml (name = packageId
;;; or id, version, fhirVersions, canonical, title, description, license, author = publisher, and the
;;; dependencies plus the core package for the FHIR version); conformance resources and
;;; `Usage: #definition` instances in package/; every other instance (FSH's default usage is
;;; #example) in package/example/; a .index.json per folder (filename, resourceType, id, url,
;;; version, kind, type), as package tooling expects. Built entirely by this compiler — no SUSHI.

(require racket/contract racket/file json file/tar
         "ig-config.rkt")

(provide (contract-out
          [pack-package! (->* (path-string? ig-config? (listof hash?) (hash/c string? string?))
                              (#:unnamed (listof string?)) path?)]
          [artifact-file-names (->* ((listof hash?)) ((listof string?)) (listof string?))]
          [conformance-type? (-> string? boolean?)]))

(define conformance-types
  '("StructureDefinition" "ValueSet" "CodeSystem" "ConceptMap" "NamingSystem" "CapabilityStatement"
    "OperationDefinition" "SearchParameter" "StructureMap" "GraphDefinition" "MessageDefinition"
    "CompartmentDefinition" "ImplementationGuide" "TerminologyCapabilities" "Library" "Measure"
    "PlanDefinition" "ActivityDefinition" "Questionnaire" "ExampleScenario" "TestScript"))
(define (conformance-type? t) (and (member t conformance-types) #t))

(define core-of (hash "4.0.1" "hl7.fhir.r4.core" "4.3.0" "hl7.fhir.r4b.core" "5.0.0" "hl7.fhir.r5.core"))

;; package-json : ig-config -> jsexpr
(define (package-json cfg)
  (define fv (or (ig-config-fhir-version cfg) "4.0.1"))
  (define deps (for/fold ([h (hasheq (string->symbol (hash-ref core-of fv "hl7.fhir.r4.core")) fv)])
                         ([d (in-list (ig-config-dependencies cfg))])
                 (hash-set h (string->symbol (car d)) (cdr d))))
  (for/fold ([h (hasheq 'name (or (ig-config-id cfg) "fsh.generated")
                        'version (or (ig-config-version cfg) "0.1.0")
                        'fhirVersions (list fv)
                        'type "fhir.ig"
                        'dependencies deps)])
            ([k '(canonical url title description license author)]
             [v (list (ig-config-canonical cfg) (ig-config-canonical cfg) (ig-config-title cfg)
                      (ig-config-description cfg) (ig-config-license cfg) (ig-config-publisher cfg))]
             #:when v)
    (hash-set h k v)))

(define (index-entry file r)
  (for/fold ([h (hasheq 'filename file 'resourceType (hash-ref r 'resourceType) 'id (hash-ref r 'id ""))])
            ([k '(url version kind type)] #:when (string? (hash-ref r k #f)))
    (hash-set h k (hash-ref r k))))

;; artifact-file-names : (listof resource) [(listof string)] -> (listof string)   <Type>-<id>.json,
;; one name per artifact. A resource with no id (an instance of a logical model, which has no `id`
;; element) is named by `unnamed` — the names of the Instances that made the id-less resources, in
;; order — when there is one name for each, else "instance". A name already given gets -2, -3 …
;; instead of overwriting the first.
(define (artifact-file-names artifacts [unnamed '()])
  (define id-less (for/sum ([a (in-list artifacts)]) (if (hash-ref a 'id #f) 0 1)))
  (define names (if (= (length unnamed) id-less) unnamed '()))
  (for/fold ([out '()] [seen (hash)] [names names] #:result (reverse out)) ([a (in-list artifacts)])
    (define id (hash-ref a 'id #f))
    (define stem (format "~a-~a" (hash-ref a 'resourceType "Resource")
                         (cond [id id] [(pair? names) (car names)] [else "instance"])))
    (define n (add1 (hash-ref seen stem 0)))
    (values (cons (if (= n 1) (format "~a.json" stem) (format "~a-~a.json" stem n)) out)
            (hash-set seen stem n)
            (if (and (not id) (pair? names)) (cdr names) names))))

;; pack-package! : out.tgz config resources usages [#:unnamed names] -> out path
;; usages: instance id -> "#example" | "#definition" (default #example)
(define (pack-package! out cfg resources usages #:unnamed [unnamed '()])
  (define dir (make-temporary-file "fsh-pack-~a" 'directory))
  (define pkg (build-path dir "package"))
  (make-directory* (build-path pkg "example"))
  (define (definitional? r)
    (define t (hash-ref r 'resourceType))
    (or (conformance-type? t)
        (equal? (hash-ref usages (hash-ref r 'id "") "#example") "#definition")))
  (define placed
    (for/list ([r (in-list resources)] [file (in-list (artifact-file-names resources unnamed))])
      (define sub (if (definitional? r) pkg (build-path pkg "example")))
      (call-with-output-file (build-path sub file) #:exists 'replace (lambda (o) (write-json r o)))
      (list sub file r)))
  (for ([sub (list pkg (build-path pkg "example"))])
    (define here (filter (lambda (p) (equal? (car p) sub)) placed))
    (call-with-output-file (build-path sub ".index.json") #:exists 'replace
      (lambda (o) (write-json (hasheq 'index-version 1 'files (for/list ([p (in-list here)]) (index-entry (cadr p) (caddr p)))) o))))
  (call-with-output-file (build-path pkg "package.json") #:exists 'replace
    (lambda (o) (write-json (package-json cfg) o)))
  (define out* (path->complete-path out))
  (when (file-exists? out*) (delete-file out*))
  (parameterize ([current-directory dir]) (tar-gzip out* "package"))
  (delete-directory/files dir)
  out*)

(module+ test
  (require rackunit)
  ;; one file name per artifact: <Type>-<id>.json; a resource with no id (an instance of a logical
  ;; model) is named by its Instance when every id-less resource has a name, else "instance"; a name
  ;; taken gets -2 (two such instances used to be written to ONE file, the later over the earlier)
  (define as (list (hasheq 'resourceType "StructureDefinition" 'id "intake")
                   (hasheq 'resourceType "intake" 'name "Ann")
                   (hasheq 'resourceType "Patient" 'id "p")
                   (hasheq 'resourceType "intake" 'name "Bob")))
  (check-equal? (artifact-file-names as '("ann-intake" "bob-intake"))
                '("StructureDefinition-intake.json" "intake-ann-intake.json" "Patient-p.json" "intake-bob-intake.json"))
  (check-equal? (artifact-file-names as)
                '("StructureDefinition-intake.json" "intake-instance.json" "Patient-p.json" "intake-instance-2.json"))
  (check-equal? (artifact-file-names as '("only-one")) (artifact-file-names as)))
