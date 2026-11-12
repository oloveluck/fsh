#lang racket/base

;;; ig.rkt — the implementation guide as a language: sushi-config.yaml compiled to the
;;; ImplementationGuide resource it states, and what is wrong with it, each problem at its line.
;;;
;;; The syntax is SUSHI's (https://fshschool.org/docs/sushi/configuration/): no new one. What the
;;; file gives is read as SUSHI reads it; what the project's other files define is listed under
;;; `definition.resource`. Pages, menus and templates are the IG Publisher's and are not written.
;;;
;;; An ImplementationGuide here is a jsexpr with, when the file gives them:
;;;   id url version name title status experimental date publisher contact description jurisdiction
;;;   copyright packageId license fhirVersion
;;;   dependsOn   one per `dependencies` entry: id, uri, packageId, version
;;;   global      one per `global` type and profile
;;;   definition  resource — one per resource of the project: its reference, name, description and
;;;               whether it is an example (R4: exampleBoolean / exampleCanonical; R5: isExample and
;;;               profile); parameter — `parameters`, as the FHIR version writes them
;;; `status` is `draft` when the file gives none (a guide under development), `name` is made from
;;; the id when the file gives none (FHIR requires both).

(require racket/contract racket/list racket/string racket/match racket/set
         (only-in fhir/diagnostic diagnostic diagnostic? loc)
         (only-in fhir/package read-package)
         (only-in fhir/suggest did-you-mean)
         (only-in "parser/ast.rkt" fsh-instance? fsh-instance-metadata)
         (only-in "compile.rkt" instance-id)
         "ig-config.rkt" "yaml.rkt")

(provide
 (contract-out
  ;; implementation-guide : ig-config resources -> jsexpr   `#:fhir-version` and `#:canonical` are
  ;; used where the file gives none
  [implementation-guide (->* (ig-config? (listof hash?))
                             (#:fhir-version (or/c string? #f) #:canonical (or/c string? #f) #:labels (hash/c string? list?))
                             hash?)]
  ;; instance-labels : entities -> (hash id -> (listof (cons title|#f description|#f)))   what FSH
  ;; Instances say of themselves (`Title:`, `Description:`), in document order under each id: the
  ;; name and description of an example in the guide, which the compiled resource has no element for
  [instance-labels (-> list? (hash/c string? list?))]
  ;; ig-diagnostics : text file -> diagnostics   `#:defined?` says whether a profile's canonical is
  ;; defined (default: every one is — undecidable without a context); `#:resource-types` are the
  ;; names `global` may constrain (default: any)
  [ig-diagnostics (->* (string? path-string?) (#:defined? (-> string? any/c) #:resource-types (or/c #f (listof string?)))
                       (listof diagnostic?))]
  ;; key-line : text key [#:under parent] -> (cons line column) | #f   where a top-level key, or a
  ;; key in the block under a top-level one, is written (1-based)
  [key-line (->* (string? string?) (#:under (or/c string? #f)) (or/c (cons/c exact-positive-integer? exact-positive-integer?) #f))]))

;; ---- reading the file's values -----------------------------------------------------------------
(define (str v) (and (string? v) (not (string=? v "")) v))
(define (strings v) (cond [(string? v) (list v)] [(list? v) (filter string? v)] [else '()]))
(define (drop-absent h) (for/hasheq ([(k v) (in-hash h)] #:unless (or (not v) (null? v) (eq? v 'absent))) (values k v)))

;; machine-name : string -> string   a name FHIR accepts (cnl-0): letters and digits, a capital first
(define (machine-name id)
  (define words (filter (lambda (w) (not (string=? w ""))) (regexp-split #rx"[^A-Za-z0-9]+" id)))
  (define joined (apply string-append (map (lambda (w) (string-append (string-upcase (substring w 0 1)) (substring w 1))) words)))
  (cond [(string=? joined "") "ImplementationGuide"]
        [(char-alphabetic? (string-ref joined 0)) joined]
        [else (string-append "IG" joined)]))

;; fsh-coding : string -> jsexpr   `system#code "display"`, as sushi-config writes a jurisdiction
(define (fsh-coding s)
  (match (regexp-match #px"^\\s*([^#\\s]*)#(\\S+)(?:\\s+\"(.*)\")?\\s*$" s)
    [(list _ system code display)
     (drop-absent (hasheq 'system (str system) 'code code 'display (str display)))]
    [_ (hasheq 'code s)]))

;; Each function below builds one element of the ImplementationGuide, and is named for it.

;; ---- contact -----------------------------------------------------------------------------------
;; contact : ig-config -> (listof ContactDetail)   the publisher's url and email, under its name
(define (contact c)
  (define publisher (match (yref (ig-config-yaml c) "publisher") [(? ymap? m) m] [(cons (? ymap? m) _) m] [_ #f]))
  (define telecom
    (for*/list ([system (in-list '("url" "email"))]
                [value (in-value (and publisher (str (yref publisher system))))] #:when value)
      (hasheq 'system system 'value value)))
  (if (null? telecom) '() (list (drop-absent (hasheq 'name (ig-config-publisher c) 'telecom telecom)))))

;; ---- dependsOn ---------------------------------------------------------------------------------
;; package-canonical : tgz -> string|#f   the `canonical` of its package.json (read once per file)
(define canonicals (make-hash))
(define (package-canonical tgz)
  (hash-ref! canonicals tgz
             (lambda ()
               (with-handlers ([exn:fail? (lambda (_) #f)])
                 (define-values (manifest _rs _es) (read-package tgz #:select (lambda (_) #f)))
                 (and (hash? manifest) (str (hash-ref manifest 'canonical #f)))))))

;; depends-on-element : DependsOn -> jsexpr   the uri is the file's, else the dependency's own guide
;; when its package is at hand, else the registry's form for a package's guide (as SUSHI writes them)
(define (depends-on-element d)
  (define package (depends-on-package d))
  (define-values (found _missing) (dependency-packages (list (cons package (depends-on-version d)))))
  (define base (and (pair? found) (package-canonical (car found))))
  (hasheq 'id (or (depends-on-id d) (regexp-replace* #rx"[^A-Za-z0-9]" package "_"))
          'uri (or (depends-on-uri d)
                   (and base (format "~a/ImplementationGuide/~a" (regexp-replace #rx"/+$" base "") package))
                   (format "http://fhir.org/packages/~a/ImplementationGuide/~a" package package))
          'packageId package
          'version (depends-on-version d)))

;; ---- definition.resource -----------------------------------------------------------------------
;; A Label is (cons title|#f description|#f): what an FSH Instance says of itself.
(define no-label (cons #f #f))

;; instance-labels : entities -> (hash id -> (listof Label))   every instance has an entry, so the
;; k-th resource with an id is the k-th instance with it (several types may share an id: IPS's
;; eumfh-39-07-1 is an AllergyIntolerance, a Condition and a Procedure)
(define (instance-labels entities)
  (for/fold ([h (hash)]) ([e (in-list entities)] #:when (fsh-instance? e))
    (define meta (fsh-instance-metadata e))
    (define (field k) (let ([p (assq k meta)]) (and p (str (cdr p)))))
    (hash-update h (instance-id e) (lambda (l) (append l (list (cons (field 'title) (field 'description))))) '())))
;; made-by-instance? : resource -> boolean   not what a Profile, ValueSet or CodeSystem entity makes
(define (made-by-instance? r)
  (not (member (hash-ref r 'resourceType #f) '("StructureDefinition" "ValueSet" "CodeSystem"))))

;; resource-element : resource r5? Label (canonical -> boolean) -> jsexpr | #f
;; A resource with a `url` is a definition, named and described by itself. Any other is an example,
;; of the first profile it claims that THIS guide defines (SUSHI: an example of a dependency's
;; profile is an example, of no profile here), named and described as its FSH Instance says.
(define (resource-element r r5? label local-profile?)
  (define type (hash-ref r 'resourceType #f))
  (define id (hash-ref r 'id #f))
  (define definition? (string? (hash-ref r 'url #f)))
  (define profile
    (and (not definition?)
         (for/first ([p (in-list (hash-ref (let ([m (hash-ref r 'meta #f)]) (if (hash? m) m (hasheq))) 'profile '()))]
                     #:when (and (string? p) (local-profile? p)))
           p)))
  (and (string? type) (string? id) (not (equal? type "ImplementationGuide"))
       (let ([base (drop-absent
                    (hasheq 'reference (hasheq 'reference (string-append type "/" id))
                            'name (if definition?
                                      (or (str (hash-ref r 'title #f)) (str (hash-ref r 'name #f)) id)
                                      (or (car label) id))
                            'description (if definition? (str (hash-ref r 'description #f)) (cdr label))))])
         (cond [r5? (let ([h (hash-set base 'isExample (not definition?))]) (if profile (hash-set h 'profile (list profile)) h))]
               [definition? (hash-set base 'exampleBoolean #f)]
               [profile (hash-set base 'exampleCanonical profile)]
               [else (hash-set base 'exampleBoolean #t)]))))

;; resource-elements : resources labels r5? -> (listof jsexpr)   one per resource, each instance-made
;; resource taking the next label of its id
(define (resource-elements resources labels r5?)
  (define local (for/set ([r (in-list resources)]
                          #:when (equal? (hash-ref r 'resourceType #f) "StructureDefinition")
                          #:when (string? (hash-ref r 'url #f)))
                  (hash-ref r 'url)))
  (define (local-profile? p) (set-member? local (car (regexp-split #rx"[|]" p))))
  (define-values (elements _left)
    (for/fold ([elements '()] [left labels]) ([r (in-list resources)])
      (define id (hash-ref r 'id #f))
      (define mine (if (and (string? id) (made-by-instance? r)) (hash-ref left id '()) '()))
      (define element (resource-element r r5? (if (pair? mine) (car mine) no-label) local-profile?))
      (values (if element (cons element elements) elements)
              (if (pair? mine) (hash-set left id (cdr mine)) left))))
  (reverse elements))

;; ---- definition.parameter ----------------------------------------------------------------------
;; R4 binds `definition.parameter.code` to ten codes; any other is the tools' ig-parameter extension
;; on `definition`. R5's code is a Coding of the tools' code system.
(define r4-parameter-codes
  '("apply" "path-resource" "path-pages" "path-tx-cache" "expansion-parameter" "rule-broken-links"
    "generate-xml" "generate-json" "generate-turtle" "html-template"))
(define ig-parameter-extension "http://hl7.org/fhir/tools/StructureDefinition/ig-parameter")
(define ig-parameter-system "http://hl7.org/fhir/tools/CodeSystem/ig-parameters")
;; parameters : ig-config -> (listof (cons code value))
(define (parameters c)
  (match (yref (ig-config-yaml c) "parameters")
    [(ymap es) (for*/list ([e (in-list es)] [v (in-list (strings (cdr e)))]) (cons (car e) v))]
    [_ '()]))

;; definition : ig-config resource-elements r5? -> jsexpr | #f   R4 requires a resource in a
;; definition: a guide that lists none has no definition yet
(define (definition c elements r5?)
  (define-values (bound unbound)
    (partition (lambda (p) (or r5? (member (car p) r4-parameter-codes))) (parameters c)))
  (and (or (pair? elements) (and r5? (pair? bound)))
       (drop-absent
        (hasheq 'extension (for/list ([p (in-list unbound)])
                             (hasheq 'url ig-parameter-extension
                                     'extension (list (hasheq 'url "code" 'valueString (car p))
                                                      (hasheq 'url "value" 'valueString (cdr p)))))
                'resource elements
                'parameter (for/list ([p (in-list bound)])
                             (hasheq 'code (if r5? (hasheq 'system ig-parameter-system 'code (car p)) (car p))
                                     'value (cdr p)))))))

;; ---- the guide ---------------------------------------------------------------------------------
(define (implementation-guide c resources #:fhir-version [fhir-version* #f] #:canonical [canonical* #f] #:labels [labels (hash)])
  (define y (ig-config-yaml c))
  (define (given k) (str (yref y k)))
  (define versions (let ([vs (ig-config-fhir-versions c)]) (if (pair? vs) vs (list (or fhir-version* "4.0.1")))))
  (define r5? (not (regexp-match? #rx"^4[.]" (car versions))))
  (define canonical (regexp-replace #rx"/+$" (or (ig-config-canonical c) canonical* "http://example.org") ""))
  (define id (or (given "id") (given "packageId") "ig"))
  (drop-absent
   (hasheq 'resourceType "ImplementationGuide"
           'id id
           'url (or (given "url") (format "~a/ImplementationGuide/~a" canonical id))
           'version (ig-config-version c)
           'name (or (ig-config-name c) (machine-name id))
           'title (ig-config-title c)
           'status (or (ig-config-status c) "draft")
           'experimental (if (eq? (ig-config-experimental c) #t) #t 'absent)
           'date (given "date")
           'publisher (ig-config-publisher c)
           'contact (contact c)
           'description (ig-config-description c)
           'jurisdiction (for/list ([j (in-list (strings (yref y "jurisdiction")))]) (hasheq 'coding (list (fsh-coding j))))
           'copyright (given "copyright")
           'packageId (or (given "packageId") id)
           'license (ig-config-license c)
           'fhirVersion versions
           'dependsOn (map depends-on-element (ig-config-depends-on c))
           'global (for/list ([g (in-list (ig-config-globals c))]) (hasheq 'type (car g) 'profile (cdr g)))
           'definition (definition c (resource-elements resources labels r5?) r5?))))

;; ---- problems ----------------------------------------------------------------------------------
;; key-line : text key [#:under parent] -> (cons line column) | #f
(define (key-line text key #:under [parent #f])
  (define lines (regexp-split #rx"\r?\n" text))
  (define (key-rx indent) (pregexp (string-append "^(" indent ")(?:-\\s+)?[\"']?" (regexp-quote key) "[\"']?\\s*:")))
  (define (find rx from to)
    (for/first ([l (in-list lines)] [n (in-naturals 1)]
                #:when (and (>= n from) (< n to))
                #:when (regexp-match? rx l))
      (cons n (add1 (string-length (cadr (regexp-match rx l)))))))
  (cond
    [(not parent) (find (key-rx "") 1 +inf.0)]
    [else
     (define top (find (key-rx "") 1 +inf.0))
     (define start (let ([p (key-line text parent)]) (and p (car p))))
     (define end (and start
                      (or (for/first ([l (in-list lines)] [n (in-naturals 1)]
                                      #:when (and (> n start) (regexp-match? #px"^[^\\s#]" l)))
                            n)
                          +inf.0)))
     (and start (find (key-rx "\\s+") (add1 start) end))]))

(define (ig-diagnostics text file #:defined? [defined? (lambda (_) #t)] #:resource-types [resource-types #f])
  (define source (if (path? file) (path->string file) file))
  (define (problem severity message at) (diagnostic 'ig 'check severity message (loc source (and at (car at)) (and at (cdr at)) #f #f)))
  (define c (with-handlers ([exn:fail:yaml? (lambda (e) e)]) (parse-ig-config text)))
  (cond
    [(exn? c)
     (list (diagnostic 'ig 'parse 'error (regexp-replace #rx"^sushi-config[.]yaml:[0-9]+: " (exn-message c) "")
                       (loc source (exn:fail:yaml-line c) 1 #f #f)))]
    [else
     (define y (ig-config-yaml c))
     (define top (cons 1 1))
     (append
      (if (or (str (yref y "id")) (str (yref y "packageId"))) '()
          (list (problem 'error "an implementation guide has an `id`: its package id, as in `id: example.clinic`" top)))
      (if (ig-config-canonical c) '()
          (list (problem 'error "an implementation guide has a `canonical`: the base of the urls it defines, as in `canonical: http://example.org/clinic`" top)))
      (if (ig-config-version c) '()
          (list (problem 'warning "no `version`: every artifact has one (`version: 0.1.0`)" top)))
      (if (ig-config-fhir-version c) '()
          (list (problem 'warning "no `fhirVersion`: 4.0.1 is used" top)))
      (let ([s (ig-config-status c)])
        (if (or (not s) (member s '("draft" "active" "retired" "unknown"))) '()
            (list (problem 'error (format "status `~a` is not draft, active, retired or unknown~a" s
                                          (did-you-mean s '("draft" "active" "retired" "unknown")))
                           (key-line text "status")))))
      ;; a dependency no package at hand provides: what it defines will not resolve
      (let-values ([(_found missing) (dependency-packages (ig-config-dependencies c))])
        (for/list ([m (in-list missing)])
          (problem 'warning
                   (format "package ~a ~a is not at hand: what it defines will not resolve (put ~a-~a.tgz in fhir-spec or on FHIR_PACKAGE_PATH)"
                           (car m) (cdr m) (car m) (cdr m))
                   (or (key-line text (car m) #:under "dependencies") (key-line text "dependencies")))))
      (for*/list ([g (in-list (ig-config-globals c))]
                  [p (in-value
                      (cond [(and resource-types (not (member (car g) resource-types)))
                             (problem 'error (format "global: `~a` is not a resource type~a" (car g) (did-you-mean (car g) resource-types))
                                      (or (key-line text (car g) #:under "global") (key-line text "global")))]
                            [(not (defined? (cdr g)))
                             (problem 'error (format "global: nothing defines the profile ~a that every ~a is held to" (cdr g) (car g))
                                      (or (key-line text (car g) #:under "global") (key-line text "global")))]
                            [else #f]))]
                  #:when p)
        p))]))

(module+ test
  (require rackunit (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line loc-column))
  (define text
    (string-append "id: example.clinic\ncanonical: http://example.org/clinic\nname: ClinicGuide\ntitle: Clinic\n"
                   "status: draft\nversion: 0.2.0\nfhirVersion: 4.0.1\nlicense: CC0-1.0\n"
                   "publisher:\n  name: Example Clinic\n  url: http://example.org\n"
                   "jurisdiction: urn:iso:std:iso:3166#US \"United States of America\"\n"
                   "dependencies:\n  hl7.fhir.us.core: 6.1.0\n  no.such.package:\n    version: 1.0.0\n    uri: http://nowhere.example/ImplementationGuide/x\n"
                   "global:\n  Observation: http://example.org/clinic/StructureDefinition/lab\n"
                   "parameters:\n  path-resource: input/extra\n  pin-canonicals: pin-all\n"))
  (define resources
    (list (hasheq 'resourceType "StructureDefinition" 'id "lab" 'url "http://example.org/clinic/StructureDefinition/lab"
                  'name "Lab" 'title "A lab result" 'description "A laboratory result.")
          (hasheq 'resourceType "Observation" 'id "a1c-1" 'meta (hasheq 'profile '("http://example.org/clinic/StructureDefinition/lab")))
          (hasheq 'resourceType "Patient" 'id "ann" 'name (list (hasheq 'family "Lee")))
          (hasheq 'resourceType "Basic")))
  (define ig (implementation-guide (parse-ig-config text) resources))
  (check-equal? (hash-ref ig 'url) "http://example.org/clinic/ImplementationGuide/example.clinic")
  (check-equal? (hash-ref ig 'packageId) "example.clinic")
  (check-equal? (hash-ref ig 'fhirVersion) '("4.0.1"))
  (check-equal? (hash-ref ig 'publisher) "Example Clinic")
  (check-equal? (hash-ref ig 'contact) (list (hasheq 'name "Example Clinic" 'telecom (list (hasheq 'system "url" 'value "http://example.org")))))
  (check-equal? (hash-ref ig 'jurisdiction)
                (list (hasheq 'coding (list (hasheq 'system "urn:iso:std:iso:3166" 'code "US" 'display "United States of America")))))
  (check-equal? (hash-ref ig 'global) (list (hasheq 'type "Observation" 'profile "http://example.org/clinic/StructureDefinition/lab")))
  ;; a dependency: its own guide's url when the package is at hand, the file's uri when it gives one
  (check-equal? (map (lambda (d) (hash-ref d 'packageId)) (hash-ref ig 'dependsOn)) '("hl7.fhir.us.core" "no.such.package"))
  (check-equal? (hash-ref (cadr (hash-ref ig 'dependsOn)) 'uri) "http://nowhere.example/ImplementationGuide/x")
  (check-equal? (hash-ref (cadr (hash-ref ig 'dependsOn)) 'id) "no_such_package")
  ;; the resources: a definition, an example of a profile, an example; a name is always a string
  (check-equal? (hash-ref (hash-ref ig 'definition) 'resource)
                (list (hasheq 'reference (hasheq 'reference "StructureDefinition/lab") 'name "A lab result"
                              'description "A laboratory result." 'exampleBoolean #f)
                      (hasheq 'reference (hasheq 'reference "Observation/a1c-1") 'name "a1c-1"
                              'exampleCanonical "http://example.org/clinic/StructureDefinition/lab")
                      (hasheq 'reference (hasheq 'reference "Patient/ann") 'name "ann" 'exampleBoolean #t)))
  ;; R4 parameters: a code FHIR binds, and one it does not (the tools' extension)
  (check-equal? (hash-ref (hash-ref ig 'definition) 'parameter) (list (hasheq 'code "path-resource" 'value "input/extra")))
  (check-equal? (hash-ref (car (hash-ref (hash-ref ig 'definition) 'extension)) 'url) ig-parameter-extension)
  ;; R5 writes examples and parameters its own way; no status, no name: draft, a name from the id
  (define ig5 (implementation-guide (parse-ig-config "id: my-guide.x\ncanonical: http://x\nfhirVersion: 5.0.0\nparameters:\n  pin-canonicals: pin-all\n")
                                    resources))
  (check-equal? (hash-ref ig5 'status) "draft")
  (check-equal? (hash-ref ig5 'name) "MyGuideX")
  (check-equal? (map (lambda (e) (hash-ref e 'isExample)) (hash-ref (hash-ref ig5 'definition) 'resource)) '(#f #t #t))
  (check-equal? (hash-ref (cadr (hash-ref (hash-ref ig5 'definition) 'resource)) 'profile) '("http://example.org/clinic/StructureDefinition/lab"))
  (check-equal? (hash-ref (car (hash-ref (hash-ref ig5 'definition) 'parameter)) 'code) (hasheq 'system ig-parameter-system 'code "pin-canonicals"))
  ;; an R4 guide that lists nothing has no definition (FHIR requires a resource in one)
  (check-false (hash-ref (implementation-guide (parse-ig-config "id: a\ncanonical: http://x\n") '()) 'definition #f))
  ;; where keys are
  (check-equal? (key-line text "status") '(5 . 1))
  (check-equal? (key-line text "no.such.package" #:under "dependencies") '(15 . 3))
  (check-equal? (key-line text "Observation" #:under "global") '(19 . 3))
  (check-false (key-line text "nope"))
  ;; problems, each at its line
  (define (lines ds) (for/list ([d (in-list ds)]) (list (diagnostic-severity d) (loc-line (diagnostic-loc d)) (diagnostic-message d))))
  (define ds (lines (ig-diagnostics text "sushi-config.yaml" #:defined? (lambda (u) #f) #:resource-types '("Observation" "Patient"))))
  (check-equal? (map car ds) '(warning error))
  (check-equal? (map cadr ds) '(15 19))
  (check-regexp-match #rx"package no.such.package 1.0.0 is not at hand" (caddr (car ds)))
  (check-regexp-match #rx"nothing defines the profile http://example.org/clinic/StructureDefinition/lab that every Observation is held to" (caddr (cadr ds)))
  (define bad (lines (ig-diagnostics "name: X\nstatus: actve\nglobal:\n  Observaton: http://x/p\n" "sushi-config.yaml" #:resource-types '("Observation" "Patient"))))
  (check-equal? (map car bad) '(error error warning warning error error))
  (check-regexp-match #rx"has an `id`" (caddr (car bad)))
  (check-regexp-match #rx"status `actve` is not draft, active, retired or unknown \\(did you mean `active`\\?\\)" (caddr (list-ref bad 4)))
  (check-equal? (cadr (list-ref bad 4)) 2)
  (check-regexp-match #rx"`Observaton` is not a resource type \\(did you mean `Observation`\\?\\)" (caddr (list-ref bad 5)))
  (check-equal? (cadr (list-ref bad 5)) 4)
  ;; what is not YAML is one problem at its line
  (define broken (ig-diagnostics "id: a\nx: &anchor y\n" "sushi-config.yaml"))
  (check-equal? (length broken) 1)
  (check-equal? (loc-line (diagnostic-loc (car broken))) 2)
  (check-equal? (machine-name "hl7.fhir.uv.ips") "Hl7FhirUvIps")
  (check-equal? (machine-name "9lives") "IG9lives"))
