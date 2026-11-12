#lang racket/base

;;; ig-workspace.rkt — the implementation guide as a workspace language (fhir/lang/workspace, id
;;; "ig"): the workspace's `sushi-config.yaml` is its ImplementationGuide as text.
;;;
;;; artifacts : the ImplementationGuide the file states (ig.rkt), listing what the workspace's other
;;;             languages define — read from fhir/registry, where a hub registers them before this
;;;             language is asked (`composes-workspace`) — with the file's problems at their lines
;;;             and the resource's own (validated, when a validator is installed)
;;; run       : the guide's facts as a summary and one row per resource it lists
;;; guide     : ig-guide.md
;;; Only the workspace root's file is the workspace's guide; one further down belongs to a project
;;; kept inside it and is left alone.

(require racket/contract racket/list racket/string racket/file racket/runtime-path
         fhir/lang/workspace
         (only-in fhir/registry registered-by registered-resource)
         (only-in fhir/diagnostic diagnostic diagnostic-severity diagnostic->jsexpr loc)
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available?)
         (only-in fhir/operation apply-operation)
         (only-in "yaml.rkt" exn:fail:yaml?)
         (only-in "ig-config.rkt" parse-ig-config)
         "ig.rkt")

(provide (contract-out [workspace-support ws-support?]
                       [guide string?]
                       [composes-workspace boolean?]))

(define-runtime-path guide-file "ig-guide.md")
(define guide (file->string guide-file))
(define composes-workspace #t)

(define (path-string p) (if (path? p) (path->string p) p))

;; guide-file-of : workspace files -> (cons path text) | #f   the root's sushi-config.yaml
(define (guide-file-of ws files)
  (define root (simplify-path (build-path (workspace-root ws) ig-file-name)))
  (for/first ([f (in-list files)] #:when (equal? (simplify-path (path->complete-path (car f))) root)) f))

;; workspace-resources : -> (listof resource)   what the other languages' files define
(define (workspace-resources)
  (registered-by (lambda (s) (and (pair? s) (eq? (car s) 'workspace) (pair? (cdr s)) (not (equal? (cadr s) "ig"))))))

;; defined? : canonical -> boolean   a profile the workspace or its packages define; without a
;; package index nothing can be said, and nothing is
(define (defined? url)
  (or (and (registered-resource "StructureDefinition" url) #t)
      (not (sd-index-available?))
      (and (with-handlers ([exn:fail? (lambda (_) #f)]) (resolve-sd-raw url)) #t)))

;; compiled : workspace (cons path text) -> jsexpr | #f   the ImplementationGuide; #f when the file
;; is not YAML
(define (compiled ws f)
  (define c (with-handlers ([exn:fail:yaml? (lambda (_) #f)]) (parse-ig-config (cdr f))))
  (define cfg (workspace-config ws))
  (and c (implementation-guide c (workspace-resources)
                               #:fhir-version (ws-config-fhir-version cfg) #:canonical (ws-config-canonical cfg))))

;; resource-problems : file text guide -> diagnostics   what $validate says of the resource, at the
;; key that states the element when the file has one (`global[0].type` -> `global`), else at the top
(define (resource-problems file text ig)
  (define oo (with-handlers ([exn:fail? (lambda (_) #f)]) (apply-operation "$validate" ig)))
  (for*/list ([i (in-list (if (hash? oo) (hash-ref oo 'issue '()) '()))]
              [sev (in-value (case (hash-ref i 'severity #f) [("error" "fatal") 'error] [else #f]))]
              #:when sev)
    (define where (let ([e (hash-ref i 'expression (hash-ref i 'location '()))]) (and (pair? e) (string? (car e)) (car e))))
    (define key (and where (let ([m (regexp-match #px"^ImplementationGuide[.]([A-Za-z]+)" where)]) (and m (cadr m)))))
    (define at (or (and key (key-line text (if (equal? key "dependsOn") "dependencies" key))) (cons 1 1)))
    (diagnostic 'ig 'validate sev
                (format "~a~a" (if where (string-append where ": ") "") (hash-ref i 'diagnostics (hash-ref i 'code "invalid")))
                (loc (path-string file) (car at) (cdr at) #f #f))))

(define (artifacts ws files)
  (define f (guide-file-of ws files))
  (cond
    [(not f) (values '() '())]
    [else
     (define ds (ig-diagnostics (cdr f) (car f) #:defined? defined?))
     (define ig (compiled ws f))
     (values (if ig (list ig) '())
             (append ds (if (and ig (not (ormap (lambda (d) (eq? (diagnostic-severity d) 'error)) ds)))
                            (resource-problems (car f) (cdr f) ig)
                            '())))]))

(define (run ws path text opts)
  (unless path (error 'ig "the implementation guide runs as the workspace's sushi-config.yaml: write it, then run its path"))
  (define ig (compiled ws (cons path text)))
  (unless ig (error 'ig "sushi-config.yaml could not be read: check it"))
  (define entries (hash-ref (hash-ref ig 'definition (hasheq)) 'resource '()))
  (define rows
    (for/list ([e (in-list entries)])
      (define ref (hash-ref (hash-ref e 'reference) 'reference))
      (hasheq 'reference ref
              'name (hash-ref e 'name 'null)
              'example (cond [(hash-ref e 'exampleCanonical #f) #t]
                             [else (hash-ref e 'exampleBoolean (hash-ref e 'isExample #f))])
              'profile (or (hash-ref e 'exampleCanonical #f)
                           (let ([p (hash-ref e 'profile '())]) (and (pair? p) (car p)))
                           'null))))
  (define ds (ig-diagnostics text path #:defined? defined?))
  (run-result "rows" '("reference" "name" "example" "profile") rows
              (hasheq 'guide (hash-ref ig 'url)
                      'version (hash-ref ig 'version 'null)
                      'status (hash-ref ig 'status)
                      'fhirVersion (hash-ref ig 'fhirVersion)
                      'dependsOn (for/list ([d (in-list (hash-ref ig 'dependsOn '()))])
                                   (format "~a#~a" (hash-ref d 'packageId) (hash-ref d 'version)))
                      'global (for/list ([g (in-list (hash-ref ig 'global '()))])
                                (format "~a: ~a" (hash-ref g 'type) (hash-ref g 'profile)))
                      'resources (length rows)
                      'byType (for/hasheq ([g (in-list (group-by (lambda (r) (car (string-split (hash-ref r 'reference) "/"))) rows))])
                                (values (string->symbol (car (string-split (hash-ref (car g) 'reference) "/"))) (length g)))
                      'problems (map diagnostic->jsexpr ds))))

(define workspace-support (ws-support "ig" artifacts run #f))

(module+ test
  (require rackunit (only-in fhir/registry register-resources! unregister-source!)
           (only-in fhir/diagnostic diagnostic-message diagnostic-loc loc-line))
  (define dir (make-temporary-directory "ig-ws-~a"))
  (define text (string-append "id: example.clinic\ncanonical: http://example.org/clinic\nversion: 0.1.0\nfhirVersion: 4.0.1\n"
                              "global:\n  Observation: http://example.org/clinic/StructureDefinition/lab\n"))
  (define f (build-path dir "sushi-config.yaml"))
  (with-output-to-file f (lambda () (display text)))
  ;; a project kept inside the workspace has its own guide: not the workspace's
  (make-directory (build-path dir "vendor"))
  (with-output-to-file (build-path dir "vendor" "sushi-config.yaml") (lambda () (display "id: other\ncanonical: http://other\n")))
  (define-values (ws _w) (open-workspace dir))
  (check-equal? (ws-config-canonical (workspace-config ws)) "http://example.org/clinic")
  ;; what the other languages define, as a hub registers it before asking this one
  (define lab (hasheq 'resourceType "StructureDefinition" 'id "lab" 'url "http://example.org/clinic/StructureDefinition/lab"
                      'name "Lab" 'title "A lab result"))
  (register-resources! (list lab (hasheq 'resourceType "Observation" 'id "a1c-1" 'meta (hasheq 'profile (list (hash-ref lab 'url)))))
                       #:source (list 'workspace "fsh"))
  (define files (list (cons f text) (cons (build-path dir "vendor" "sushi-config.yaml") "id: other\ncanonical: http://other\n")))
  (define-values (rs ds) (artifacts ws files))
  (check-equal? (length rs) 1)
  (check-equal? (hash-ref (car rs) 'url) "http://example.org/clinic/ImplementationGuide/example.clinic")
  (check-equal? (map (lambda (e) (hash-ref (hash-ref e 'reference) 'reference)) (hash-ref (hash-ref (car rs) 'definition) 'resource))
                '("Observation/a1c-1" "StructureDefinition/lab"))
  (check-equal? (map diagnostic-message ds) '())
  (define r (run ws f text (hasheq)))
  (check-equal? (run-result-columns r) '("reference" "name" "example" "profile"))
  (check-equal? (map (lambda (row) (hash-ref row 'example)) (run-result-rows r)) '(#t #f))
  (check-equal? (hash-ref (run-result-summary r) 'global) '("Observation: http://example.org/clinic/StructureDefinition/lab"))
  ;; the profile gone: the global rule names nothing, at its line
  (unregister-source! (list 'workspace "fsh"))
  (define-values (_rs ds2) (artifacts ws files))
  (when (sd-index-available?)
    (check-equal? (length ds2) 1)
    (check-regexp-match #rx"nothing defines the profile" (diagnostic-message (car ds2)))
    (check-equal? (loc-line (diagnostic-loc (car ds2))) 6))
  (check-true composes-workspace)
  (check-regexp-match #rx"sushi-config.yaml" guide)
  (delete-directory/files dir))
