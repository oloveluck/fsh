#lang racket/base

;;; ig-test.rkt — the implementation guide as a language (ig.rkt, ig-workspace.rkt, ig-guide.md):
;;;
;;; 1. The guide's claims: every ```yaml block compiles without an error; the worked example — the
;;;    cheat sheet's guide and the ```fsh block as one workspace — answers what the ```answer block
;;;    says; the messages the guide quotes are the ones the checker gives.
;;; 2. Published guides as the oracle: IPS 2.0.1 and mCODE 4.0.0 from their own sushi-config.yaml
;;;    and FSH, against the ImplementationGuide in the published package (SUSHI's output after the
;;;    IG Publisher). Held: the header elements; every dependency the file states, with the url and
;;;    id published; every resource listed, none more; which are examples. Ratcheted: of which
;;;    profile (the Publisher's `|version` pin aside; an instance claiming two local profiles is an
;;;    example of its InstanceOf, which the compiled resource does not say — IPS has one), names
;;;    and descriptions.

(require rackunit racket/file racket/list racket/string racket/runtime-path json
         (only-in fhir/lang/workspace open-workspace ws-support-run ws-support-artifacts run-result-summary run-result-rows)
         (only-in fhir/registry register-resources! unregister-source!)
         (only-in fhir/package read-package)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/diagnostic diagnostic-severity diagnostic-message)
         "../project.rkt" "../ig.rkt" "../ig-config.rkt" (only-in "../ig-workspace.rkt" workspace-support))

(define-runtime-path guide-file "../ig-guide.md")
(define-runtime-path vendor "vendor")
(define guide-text (file->string guide-file))

;; blocks : language -> (listof text)   the guide's fenced blocks of one language
(define (blocks lang)
  (map cadr (regexp-match* (pregexp (string-append "```" lang "\n(.*?)```")) guide-text #:match-select values)))

;; ---- 1. the guide ----------------------------------------------------------------------------
(define yaml-blocks (blocks "yaml"))
(check-equal? (length yaml-blocks) 1)
(for ([b (in-list yaml-blocks)])
  (check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (ig-diagnostics b "sushi-config.yaml")) '()))

;; subset? : expected actual -> boolean   every key and item the guide shows is in the answer
(define (subset? e a)
  (cond [(hash? e) (and (hash? a) (for/and ([(k v) (in-hash e)]) (and (hash-has-key? a k) (subset? v (hash-ref a k)))))]
        [(list? e) (and (list? a) (= (length e) (length a)) (andmap subset? e a))]
        [else (equal? e a)]))

(let ([dir (make-temporary-directory "ig-guide-~a")])
  (define config (car yaml-blocks))
  (with-output-to-file (build-path dir "sushi-config.yaml") (lambda () (display config)))
  (with-output-to-file (build-path dir "lab.fsh") (lambda () (display (car (blocks "fsh")))))
  (define p (compile-project dir))
  (check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (project-diagnostics p)) '())
  ;; as a hub does: the other languages' resources registered, then the guide asked
  (register-resources! (project-artifacts p) #:source (list 'workspace "fsh"))
  (define-values (ws _w) (open-workspace dir))
  (define r ((ws-support-run workspace-support) ws (build-path dir "sushi-config.yaml") config (hasheq)))
  (define answer (string->jsexpr (car (blocks "answer"))))
  (define actual (hasheq 'summary (run-result-summary r)
                         'rows (for/list ([row (in-list (run-result-rows r))])
                                 (for/hasheq ([(k v) (in-hash row)] #:unless (eq? v 'null)) (values k v)))))
  (check-true (subset? answer (string->jsexpr (jsexpr->string actual)))
              (format "the guide's answer is not what the run gives: ~a" (jsexpr->string actual)))
  ;; the url the text states for the profile, and the resource `describe` would give
  (define-values (rs ds) ((ws-support-artifacts workspace-support) ws (list (cons (build-path dir "sushi-config.yaml") config))))
  (check-equal? (map diagnostic-message (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) ds)) '())
  (check-equal? (hash-ref (car rs) 'url) "http://example.org/clinic/ImplementationGuide/example.clinic")
  (check-regexp-match #rx"http://example.org/clinic/StructureDefinition/lab" guide-text)
  (unregister-source! (list 'workspace "fsh"))
  (delete-directory/files dir))

;; the messages the guide quotes: each one the checker gives starts as the guide writes it
(define (quoted? message) (and (regexp-match? (regexp-quote (substring message 0 (min 34 (string-length message)))) guide-text) #t))
(for ([d (in-list (append (ig-diagnostics "status: actve\ndependencies:\n  no.such.package: 1.0.0\n" "sushi-config.yaml")
                          (ig-diagnostics "id: a\ncanonical: http://x\nversion: 1.0.0\nfhirVersion: 4.0.1\nglobal:\n  Observation: http://x/StructureDefinition/nope\n"
                                          "sushi-config.yaml" #:defined? (lambda (_) #f))))]
      #:unless (regexp-match? #rx"^no `fhirVersion`" (diagnostic-message d)))
  (check-true (quoted? (diagnostic-message d)) (diagnostic-message d)))

;; ---- 2. published guides ---------------------------------------------------------------------
;; published-guide : package-file -> jsexpr   the ImplementationGuide in a published package
(define (published-guide package)
  (define-values (_m rs _es) (read-package (spec-package-path package) #:select (lambda (n) (regexp-match? #rx"ImplementationGuide-" n))))
  (for/first ([r (in-list rs)] #:when (equal? (hash-ref (if (pair? r) (cdr r) r) 'resourceType #f) "ImplementationGuide"))
    (if (pair? r) (cdr r) r)))
(define (entries g)
  (for/hash ([e (in-list (hash-ref (hash-ref g 'definition (hasheq)) 'resource '()))])
    (values (hash-ref (hash-ref e 'reference) 'reference) e)))
(define (unpinned v) (if (string? v) (car (regexp-split #rx"[|]" v)) v))

;; check-against-published : name dir package profile-floor name-floor description-floor -> void
(define (check-against-published name dir package profile-floor name-floor description-floor)
  (define p (compile-project (build-path vendor dir)))
  (define ours (implementation-guide (project-config p) (project-artifacts p) #:fhir-version (project-fhir-version p)
                                     #:labels (instance-labels (map car (project-located p)))))
  (define theirs (published-guide package))
  (for ([k (in-list '(id url version name title status packageId license fhirVersion publisher contact jurisdiction))])
    (check-equal? (hash-ref ours k #f) (hash-ref theirs k #f) (format "~a: ~a" name k)))
  ;; every dependency the file states is published as we write it (the Publisher adds its own)
  (define (dependency d) (list (hash-ref d 'packageId) (hash-ref d 'version) (hash-ref d 'uri) (hash-ref d 'id)))
  (for ([d (in-list (hash-ref ours 'dependsOn '()))])
    (check-not-false (member (dependency d) (map dependency (hash-ref theirs 'dependsOn '()))) (format "~a: dependsOn ~a" name (dependency d))))
  (define o (entries ours))
  (define t (entries theirs))
  (check-equal? (sort (hash-keys o) string<?) (sort (hash-keys t) string<?) (format "~a: the resources listed" name))
  (define (agree field [norm values])
    (count (lambda (k) (equal? (norm (hash-ref (hash-ref o k) field #f)) (norm (hash-ref (hash-ref t k) field #f)))) (hash-keys o)))
  (check-equal? (agree 'exampleBoolean) (hash-count o) (format "~a: which are examples" name))
  (define profiles (agree 'exampleCanonical unpinned))
  (define names (agree 'name))
  (define descriptions (agree 'description))
  (printf "ImplementationGuide ~a: ~a resources listed as published; example profiles ~a, names ~a, descriptions ~a\n"
          name (hash-count o) profiles names descriptions)
  (check-true (>= profiles profile-floor) (format "~a: example profiles ~a < ~a" name profiles profile-floor))
  (check-true (>= names name-floor) (format "~a: names ~a < ~a" name names name-floor))
  (check-true (>= descriptions description-floor) (format "~a: descriptions ~a < ~a" name descriptions description-floor)))

(check-against-published "IPS 2.0.1" "ips-2.0.1" "hl7.fhir.uv.ips.tgz" 116 94 90)
(check-against-published "mCODE 4.0.0" "mcode-4.0.0" "hl7.fhir.us.mcode-4.0.0.tgz" 349 348 348)
