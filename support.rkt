#lang racket/base

;;; support.rkt — what FSH offers the family's editor and agent hubs (fhir/lang/support): diagnostics
;;; for a document with its project, the project's files, the outline, hover on element paths and
;;; names, go to definition across the project, and completion. The language server loads this module
;;; by its collection name; nothing here knows the protocol. What an agent runs is workspace.rkt.

(require racket/contract racket/list
         fhir/lang/support
         (only-in "project.rkt" compile-project project-diagnostics)
         "support/symbols.rkt" "support/complete.rkt")

(provide (contract-out [support lang-support?]))

(define (path-string p) (if (path? p) (path->string p) p))

;; diagnose : text path -> diagnostics   the document's project compiled (sushi-config beside it or
;; up to two levels up), with this text standing in for the file on disk
(define (diagnose text path)
  (define p (compile-project (project-root path) #:texts (hash (path-string (simplify-path (path->complete-path path))) (blank-lang-line text))))
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

(define support (lang-support "fsh" diagnose project-files entities-of hover definition completion))

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
  ;; a Parent another file of the project defines is found (the document alone used to be compiled,
  ;; so the editor said "Parent `P2` not found" while go to definition opened it)
  (with-output-to-file (build-path dir "a.fsh") #:exists 'truncate (lambda () (display "Profile: P2\nParent: Observation\n")))
  (with-output-to-file (build-path dir "b.fsh") (lambda () (display "Profile: Q\nParent: P2\n")))
  (check-equal? (diagnose "Profile: Q\nParent: P2\n* status MS\n" (build-path dir "b.fsh")) '())
  (delete-directory/files dir))
