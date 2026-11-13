#lang racket/base

;;; workspace.rkt — what FSH offers a workspace (fhir/lang/workspace): the workspace's FSH compiled as
;;; one project (its canonical, FHIR version and packages from workspace.yaml; a sushi-config.yaml
;;; still read when there is one), every example validated against the profiles, the problems
;;; positioned in the FSH; a file's run is what it defines; an explanation, the problems of one entity.
;;;
;;; A Row of a run is {resourceType, id, url, errors, warnings, resource}.
;;; An explanation is {entity, kind, line, resource, problems: [diagnostic]} — the problems positioned
;;; inside the entity (from its declaration to the next one), an example's validation issues among
;;; them, each traced to the rule that wrote the element.

(require racket/contract racket/list racket/string racket/file racket/runtime-path
         fhir/lang/workspace
         (only-in fhir/lang/support entity-kind entity-name entity-id entity-line)
         (only-in fhir/diagnostic diagnostic-loc diagnostic-message diagnostic-severity loc-source loc-line diagnostic->jsexpr)
         (only-in "check.rkt" check-fsh-project check-report-diagnostics check-report-artifacts)
         (only-in "support/symbols.rkt" entities-of))

(provide (contract-out [workspace-support ws-support?]
                       ;; guide : the language guide (guide.md) a hub serves to agents (learn://fsh)
                       [guide string?]))

(define-runtime-path guide-file "guide.md")
(define guide (file->string guide-file))

(define (path-string p) (if (path? p) (path->string p) p))

;; report : workspace -> check-report   the workspace's FSH as one project
(define (report ws)
  (define-values (pkgs _missing) (workspace-packages ws))
  (define cfg (workspace-config ws))
  (check-fsh-project (workspace-root ws) #:canonical (ws-config-canonical cfg) #:fhir-version (ws-config-fhir-version cfg)
                     #:packages pkgs #:profiles (ws-config-profiles cfg)
                     ;; the workspace's own files: not its data directory, nor the text export of its store
                     #:files (workspace-files ws)))

(define (artifacts ws files)
  (cond [(null? files) (values '() '())]
        [else (define r (report ws))
              (values (check-report-artifacts r) (check-report-diagnostics r))]))

;; the entities a text declares that compile to a resource, with each one's line span [from, to)
(define (spans text path)
  (define es (filter (lambda (e) (member (entity-kind e) '("Profile" "Extension" "Logical" "Resource" "Instance" "ValueSet" "CodeSystem")))
                     (entities-of text path)))
  (define all-lines (sort (map entity-line (entities-of text path)) <))
  (for/list ([e (in-list es)])
    (define next (for/first ([l (in-list all-lines)] #:when (> l (entity-line e))) l))
    (list e (add1 (entity-line e)) (if next (add1 next) +inf.0))))

;; artifact-of : entity artifacts -> resource|#f   by id, else by name
(define (artifact-of e artifacts)
  (define kind (case (entity-kind e) [("Profile" "Extension" "Logical" "Resource") "StructureDefinition"] [("Instance") #f] [else (entity-kind e)]))
  (define (kind-ok? a) (or (not kind) (equal? (hash-ref a 'resourceType #f) kind)))
  (or (for/first ([a (in-list artifacts)] #:when (and (kind-ok? a) (entity-id e) (equal? (hash-ref a 'id #f) (entity-id e)))) a)
      (for/first ([a (in-list artifacts)] #:when (and (kind-ok? a) (equal? (hash-ref a 'id #f) (entity-name e)))) a)
      (for/first ([a (in-list artifacts)] #:when (and (kind-ok? a) (equal? (hash-ref a 'name #f) (entity-name e)))) a)))

;; problems-in : diagnostics path from to -> diagnostics
(define (problems-in ds path from to)
  (define p (path-string (simplify-path (path->complete-path path))))
  (for/list ([d (in-list ds)]
             #:when (let ([l (diagnostic-loc d)])
                      (and l (loc-source l) (equal? (path-string (simplify-path (path->complete-path (loc-source l)))) p)
                           (loc-line l) (<= from (loc-line l)) (< (loc-line l) to))))
    d))

(define (run ws path text opts)
  (unless path (error 'fsh "FSH runs as a file of the workspace: write it, then run its path"))
  (define r (report ws))
  (define ds (check-report-diagnostics r))
  (define rows
    (for*/list ([s (in-list (spans text path))]
                [a (in-value (artifact-of (car s) (check-report-artifacts r)))] #:when a)
      (define ps (problems-in ds path (cadr s) (caddr s)))
      (hasheq 'resourceType (hash-ref a 'resourceType) 'id (hash-ref a 'id 'null) 'url (hash-ref a 'url 'null)
              'errors (count (lambda (d) (eq? (diagnostic-severity d) 'error)) ps)
              'warnings (count (lambda (d) (eq? (diagnostic-severity d) 'warning)) ps)
              'resource a)))
  (define file-ds (problems-in ds path 0 +inf.0))
  (run-result "resources" '("resourceType" "id" "url" "errors" "warnings" "resource") rows
              (hasheq 'resources (length rows)
                      'byType (for/hasheq ([g (in-list (group-by (lambda (row) (hash-ref row 'resourceType)) rows))])
                                (values (string->symbol (hash-ref (car g) 'resourceType)) (length g)))
                      'errors (count (lambda (d) (eq? (diagnostic-severity d) 'error)) file-ds)
                      'problems (map diagnostic->jsexpr file-ds))))

(define (explain ws path text subject opts)
  (unless path (error 'fsh "FSH explains a file of the workspace"))
  (define s (for/first ([s (in-list (spans text path))]
                        #:when (or (equal? (entity-name (car s)) subject) (equal? (entity-id (car s)) subject)))
              s))
  (unless s (error 'fsh (format "~a declares no entity named ~a (it declares: ~a)" (path-string path) subject
                                (string-join (map (lambda (x) (entity-name (car x))) (spans text path)) ", "))))
  (define r (report ws))
  (define e (car s))
  (hasheq 'entity (entity-name e) 'kind (entity-kind e) 'line (cadr s)
          'resource (or (artifact-of e (check-report-artifacts r)) 'null)
          'problems (map diagnostic->jsexpr (problems-in (check-report-diagnostics r) path (cadr s) (caddr s)))))

(define workspace-support (ws-support "fsh" artifacts run explain))

(module+ test
  (require rackunit racket/file)
  (define dir (make-temporary-directory "fsh-ws-~a"))
  (with-output-to-file (build-path dir "workspace.yaml") (lambda () (display "fhirVersion: 4.0.1\ncanonical: http://example.org/ws\n")))
  (define text (string-append "Profile: BP\nParent: Observation\nId: bp\n* status = #final\n\n"
                              "Instance: bp-ok\nInstanceOf: bp\n* status = #final\n* code = http://loinc.org#85354-9\n\n"
                              "Instance: bp-bad\nInstanceOf: bp\n* code = http://loinc.org#85354-9\n* status = #preliminary\n"))
  (define f (build-path dir "bp.fsh"))
  (with-output-to-file f (lambda () (display text)))
  (define-values (ws _) (open-workspace dir))
  (define-values (rs ds) (artifacts ws (list (cons f text))))
  (check-true (and (findf (lambda (a) (equal? (hash-ref a 'url #f) "http://example.org/ws/StructureDefinition/bp")) rs) #t))
  (define r (run ws f text (hasheq)))
  (check-equal? (map (lambda (row) (hash-ref row 'id)) (run-result-rows r)) '("bp" "bp-ok" "bp-bad"))
  (check-equal? (map (lambda (row) (hash-ref row 'errors)) (run-result-rows r)) '(0 0 1))
  (define e (explain ws f text "bp-bad" (hasheq)))
  (check-equal? (hash-ref e 'kind) "Instance")
  (check-equal? (hash-ref e 'line) 11)
  (check-equal? (length (hash-ref e 'problems)) 1)
  (check-regexp-match #rx"final" (hash-ref (car (hash-ref e 'problems)) 'message))
  (check-exn #rx"declares no entity named nope" (lambda () (explain ws f text "nope" (hasheq))))
  (delete-directory/files dir)
  ;; workspace.yaml's profiles hold the examples to them as they hold staged data: an Observation
  ;; that claims nothing still breaks the workspace's rule for Observations
  (define dir2 (make-temporary-directory "fsh-ws-~a"))
  (with-output-to-file (build-path dir2 "workspace.yaml")
    (lambda () (display "fhirVersion: 4.0.1\ncanonical: http://example.org/ws\nprofiles:\n  Observation: http://example.org/ws/StructureDefinition/bp\n")))
  (define text2 (string-append "Profile: BP\nParent: Observation\nId: bp\n* status = #final\n\n"
                               "Instance: plain\nInstanceOf: Observation\n* code = http://loinc.org#85354-9\n* status = #preliminary\n"))
  (define f2 (build-path dir2 "bp.fsh"))
  (with-output-to-file f2 (lambda () (display text2)))
  (define-values (ws2 _2) (open-workspace dir2))
  (define-values (rs2 ds2) (artifacts ws2 (list (cons f2 text2))))
  (check-true (for/or ([d (in-list ds2)]) (regexp-match? #rx"final" (diagnostic-message d))) (format "~s" (map diagnostic-message ds2)))
  (delete-directory/files dir2))
