#lang racket/base

;;; support.rkt — what FSH offers the family's editor and agent hubs (fhir/lang/support): diagnostics
;;; for a document with its project, the project's files, the outline, hover on element paths and
;;; names, go to definition across the project, completion, and the tools an agent calls (check a
;;; project, compile FSH, decompile FHIR). The language server and the MCP server load this module by
;;; its collection name; nothing here knows the protocols.

(require racket/contract racket/string racket/list json
         fhir/lang/support
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available?)
         (only-in "project.rkt" compile-project project-diagnostics)
         (only-in "check.rkt" check-fsh-project check-report-diagnostics)
         (only-in "compile.rkt" fsh->fhir)
         (only-in "decompile.rkt" fhir->fsh)
         "support/symbols.rkt" "support/complete.rkt")

(provide (contract-out [support lang-support?]))

(define (path-string p) (if (path? p) (path->string p) p))

;; diagnose : text path -> diagnostics   the document's project compiled (sushi-config beside it or
;; up to two levels up), with this text standing in for the file on disk
(define (diagnose text path)
  (define p (compile-project path #:texts (hash (path-string (simplify-path (path->complete-path path))) (blank-lang-line text))))
  (project-diagnostics p))

;; project-files : path -> paths   every .fsh of the document's project
(define (project-files path) (project-fsh-files (project-root path)))

;; project-aliases : project -> (hash alias -> value)   the Alias lines of every project file
(define (project-aliases pr)
  (for*/fold ([h (hash)]) ([f (in-list (remove-duplicates (map entity-file (project-entities pr))))]
                           [(k v) (in-hash (aliases-of ((project-text-of pr) f)))])
    (hash-set h k v)))

;; hover : project text position -> markdown|#f   a rule's element path in the base type, or the
;; declaration a name refers to
(define (hover pr text pos)
  (define es (project-entities pr))
  (define aliases (project-aliases pr))
  (define rule-path (rule-path-at text pos))
  (cond
    [rule-path (let ([base (parent-of text es aliases)]) (and base (element-hover base rule-path)))]
    [else (define w (word-at text pos))
          (define e (and w (find-entity es (hash-ref aliases w w))))
          (and e (entity-hover e ((project-text-of pr) (entity-file e))))]))

;; definition : project text position -> entity|#f   the project's declaration the name under the
;; cursor refers to (a Parent, an InstanceOf, a value set, an alias, a RuleSet, an Invariant, an instance)
(define (definition pr text pos)
  (define w (word-at text pos))
  (and w (find-entity (project-entities pr) (hash-ref (project-aliases pr) w w))))

(define (completion pr text pos) (complete text pos (project-entities pr) (project-aliases pr)))

;; ---- the tools --------------------------------------------------------------------------------
(define tools
  (list
   (tool "fsh_check"
         "Check an FSH project (a directory with sushi-config.yaml and .fsh files): compile it and validate every example against its profiles. Returns the diagnostics, each positioned in the FSH."
         (text-schema 'directory "the project directory")
         (lambda (args)
           (define dir (string-arg args 'directory))
           (cond [(not dir) (missing 'directory)]
                 [(not (directory-exists? dir)) (values (format "no such directory: ~a" dir) #t)]
                 [else (diagnostics-result (check-report-diagnostics (check-fsh-project dir)))])))
   (tool "fsh_compile"
         "Compile FSH text (profiles, extensions, instances, value sets, code systems) to FHIR JSON resources. R5 with the default canonical base."
         (text-schema 'fsh "the FSH source")
         (lambda (args)
           (define src (string-arg args 'fsh))
           (cond [(not src) (missing 'fsh)]
                 [else (values (jsexpr->string (fsh->fhir src)) #f)])))
   (tool "fsh_decompile"
         "FHIR JSON to FSH (GoFSH-style): a StructureDefinition, ValueSet or CodeSystem becomes idiomatic FSH, any other resource an Instance."
         (hasheq 'type "object" 'properties (hasheq 'resource (hasheq 'type "object" 'description "the resource as JSON")) 'required '("resource"))
         (lambda (args)
           (define r (hash-ref args 'resource #f))
           (cond [(not (hash? r)) (missing 'resource)]
                 [else (dynamic-require 'fhir/r5/schemas #f)
                       (values (fhir->fsh r) #f)])))))

(define support (lang-support "fsh" diagnose project-files entities-of hover definition completion tools))

(module+ test
  (require rackunit racket/file (only-in fhir/r5/schemas))
  ;; a project from texts in memory: hover, definition and completion over its entities
  (define text "Alias: $loinc = http://loinc.org\n\nProfile: BP\nParent: Observation\nId: bp\n* component[systolic].code = $loinc#8480-6\n\nInstance: a\nInstanceOf: bp\n* status = #final\n")
  (define pr (project "/tmp/nowhere/bp.fsh" (entities-of text "/tmp/nowhere/bp.fsh") (lambda (f) text)))
  (check-regexp-match #rx"Observation.component.code" (hover pr text (cons 5 5)))
  (check-regexp-match #rx"Profile: BP" (hover pr text (cons 8 13)))
  (check-equal? (entity-name (definition pr text (cons 8 13))) "BP")
  (check-true (and (member "BP" (map (lambda (c) (hash-ref c 'label)) (completion pr text (cons 3 10)))) #t))
  ;; a project on disk: its files, and the diagnostics of one (the compile sees the project)
  (define dir (make-temporary-file "fsh-support-~a" 'directory))
  (with-output-to-file (build-path dir "sushi-config.yaml") (lambda () (display "canonical: http://x\nfhirVersion: 5.0.0\n")))
  (with-output-to-file (build-path dir "a.fsh") (lambda () (display "Profile: P\nParent: Observation\n* nmae MS\n")))
  (check-equal? (length (project-files (build-path dir "a.fsh"))) 1)
  (define ds (diagnose "Profile: P\nParent: Observation\n* nmae MS\n" (build-path dir "a.fsh")))
  (check-true (pair? ds))
  (delete-directory/files dir)
  ;; the tools
  (define by-name (for/hash ([t (in-list tools)]) (values (tool-name t) t)))
  (define-values (t1 e1) (call-tool (hash-ref by-name "fsh_compile") (hasheq 'fsh "Profile: P\nParent: Patient\n* name MS\n")))
  (check-false e1)
  (check-equal? (hash-ref (car (string->jsexpr t1)) 'resourceType) "StructureDefinition")
  (define-values (t2 e2) (call-tool (hash-ref by-name "fsh_compile") (hasheq)))
  (check-true e2)
  (define-values (t3 e3) (call-tool (hash-ref by-name "fsh_check") (hasheq 'directory "/nonexistent")))
  (check-true e3))
