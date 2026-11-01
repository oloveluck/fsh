#lang racket/base

;;; ig-config.rkt — what an FSH project's sushi-config.yaml says about compiling it.
;;;
;;; SUSHI reads `canonical`, `fhirVersion` and `dependencies` (the packages whose profiles,
;;; extensions, value sets and code systems the FSH builds on: `Parent: USCorePatientProfile`). We read
;;; exactly those three keys with a line-oriented reader for the shapes sushi-config uses — a full
;;; YAML parser is not needed for them — and locate each dependency's NPM package through fhir-spec
;;; (`<id>-<version>.tgz`, or `<id>.tgz` when that package IS the version asked for).
;;;
;;; A Dependency is (cons package-id version), e.g. ("hl7.fhir.us.core" . "6.1.0"). Both YAML forms:
;;;   dependencies:
;;;     hl7.fhir.uv.genomics-reporting: 2.0.0
;;;     hl7.fhir.us.core:
;;;       version: 6.1.0
;;;       id: hl7fhiruscore

(require racket/contract racket/string racket/list racket/file racket/match
         json file/gunzip file/untar
         (only-in fhir/spec-package spec-package-path))

(provide
 (struct-out ig-config)
 (contract-out
  [read-ig-config      (-> path-string? ig-config?)]
  [dependency-packages (-> (listof (cons/c string? string?)) (values (listof path?) (listof (cons/c string? string?))))]))

;; ig-config : canonical fhir-version dependencies — any may be #f / '()
(struct ig-config (canonical fhir-version dependencies) #:transparent)

;; read-ig-config : path -> ig-config
(define (read-ig-config file)
  (define lines (file->lines file))
  (define (top key)
    (for/or ([l (in-list lines)])
      (define m (regexp-match (pregexp (string-append "^" (regexp-quote key) ":\\s*(.*?)\\s*$")) l))
      (and m (not (string=? (unquote* (cadr m)) "")) (unquote* (cadr m)))))
  (ig-config (top "canonical") (top "fhirVersion") (read-dependencies lines)))

(define (unquote* s) (string-trim (regexp-replace #rx"[ \t]+#.*$" s "") #px"[\"' ]"))
(define (indent l) (string-length (car (regexp-match #px"^ *" l))))
(define (blank-or-comment? l) (regexp-match? #px"^\\s*(#.*)?$" l))

;; read-dependencies : (listof string) -> (listof Dependency)
(define (read-dependencies lines)
  (define body
    (let loop ([ls lines])
      (cond [(null? ls) '()]
            [(regexp-match? #px"^dependencies:\\s*$" (car ls))
             (for/list ([l (in-list (cdr ls))]
                        #:break (and (not (blank-or-comment? l)) (= (indent l) 0))
                        #:unless (blank-or-comment? l))
               l)]
            [else (loop (cdr ls))])))
  (define base (if (null? body) 0 (indent (car body))))
  (let loop ([ls body] [acc '()])
    (match ls
      ['() (reverse acc)]
      [(cons l rest)
       (match (regexp-match #px"^\\s*([^:\\s]+):\\s*(.*?)\\s*$" l)
         [(list _ id v) #:when (= (indent l) base)
          (define inline (unquote* v))
          (define nested (for/list ([n (in-list rest)] #:break (<= (indent n) base)) n))
          (define version
            (if (string=? inline "")
                (for/or ([n (in-list nested)])
                  (define m (regexp-match #px"^\\s*version:\\s*(.*?)\\s*$" n))
                  (and m (unquote* (cadr m))))
                inline))
          (loop (list-tail rest (length nested)) (if version (cons (cons id version) acc) acc))]
         [_ (loop rest acc)])])))

;; dependency-packages : (listof Dependency) -> (values (listof tgz-path) (listof missing Dependency))
(define (dependency-packages deps)
  (for/fold ([found '()] [missing '()] #:result (values (reverse found) (reverse missing)))
            ([d (in-list deps)])
    (define p (or (spec-package-path (format "~a-~a.tgz" (car d) (cdr d)) #:optional? #t)
                  (let ([q (spec-package-path (format "~a.tgz" (car d)) #:optional? #t)])
                    (and q (equal? (package-version q) (cdr d)) q))))
    (if p (values (cons p found) missing) (values found (cons d missing)))))

;; package-version : tgz -> string|#f   package/package.json's version
(define (package-version tgz)
  (define v #f)
  (call-with-input-file tgz
    (lambda (gz)
      (define-values (pin pout) (make-pipe))
      (define t (thread (lambda () (gunzip-through-ports gz pout) (close-output-port pout))))
      (untar pin #:handle-entry
             (lambda (k n c sz a)
               (when (and (eq? k 'file) (equal? (path->string n) "package/package.json"))
                 (set! v (hash-ref (bytes->jsexpr (read-bytes sz c)) 'version #f)))
               (when (eq? k 'file) (void (read-bytes sz c)))
               '()))
      (thread-wait t)))
  v)

(module+ test
  (require rackunit)
  (define f (make-temporary-file))
  (with-output-to-file f #:exists 'truncate
    (lambda () (display "canonical: http://hl7.org/fhir/us/mcode\nfhirVersion: 4.0.1\ndependencies:\n  hl7.fhir.uv.genomics-reporting: 2.0.0\n#  hl7.fhir.uv.extensions: 5.1.0-cibuild\n  hl7.fhir.us.core: \n    version: 6.1.0\n    id: hl7fhiruscore\nextension:\n  - url: x\n")))
  (define c (read-ig-config f))
  (check-equal? (ig-config-canonical c) "http://hl7.org/fhir/us/mcode")
  (check-equal? (ig-config-fhir-version c) "4.0.1")
  (check-equal? (ig-config-dependencies c) '(("hl7.fhir.uv.genomics-reporting" . "2.0.0") ("hl7.fhir.us.core" . "6.1.0"))))
