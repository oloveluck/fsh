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
         (only-in fhir/spec-package spec-package-path current-package-directories))

(provide
 (struct-out ig-config)
 (contract-out
  [read-ig-config      (-> path-string? ig-config?)]
  [dependency-packages (-> (listof (cons/c string? string?)) (values (listof path?) (listof (cons/c string? string?))))]
  ;; implicit-packages : fhir-version -> (listof tgz-path)   the packages SUSHI loads without being
  ;; asked — hl7.fhir.uv.extensions and hl7.terminology for the version — when a copy is at hand
  ;; (current-package-directories, then fhir-spec; the newest version of each), else none
  [implicit-packages (-> (or/c string? #f) (listof path?))]))

;; ig-config : what compiling and packaging read — any may be #f / '()
;;   canonical fhir-version dependencies   (compile)
;;   id name title version status license description publisher   (package.json; publisher = its name)
(struct ig-config (canonical fhir-version dependencies id name title version status license description publisher)
  #:transparent)

;; read-ig-config : path -> ig-config
(define (read-ig-config file)
  (define lines (file->lines file))
  (define (top key)
    (for/or ([l (in-list lines)])
      (define m (regexp-match (pregexp (string-append "^" (regexp-quote key) ":\\s*(.*?)\\s*$")) l))
      (and m (not (string=? (unquote* (cadr m)) "")) (unquote* (cadr m)))))
  (define publisher
    (or (top "publisher")
        (let loop ([ls lines])
          (cond [(null? ls) #f]
                [(regexp-match? #px"^publisher:\\s*$" (car ls))
                 (for/or ([l (in-list (cdr ls))] #:break (and (not (blank-or-comment? l)) (= (indent l) 0)))
                   (define m (regexp-match #px"^\\s+-?\\s*name:\\s*(.*?)\\s*$" l))
                   (and m (unquote* (cadr m))))]
                [else (loop (cdr ls))]))))
  (ig-config (top "canonical") (top "fhirVersion") (read-dependencies lines)
             (or (top "packageId") (top "id")) (top "name") (top "title") (top "version") (top "status")
             (top "license") (top "description") publisher))

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

;; the implicit packages, by the suffix the registry gives their FHIR-version builds
(define (implicit-packages fhir-version)
  (define suffix (cond [(and fhir-version (regexp-match? #rx"^4[.]" fhir-version)) "r4"]
                       [(and fhir-version (regexp-match? #rx"^5[.]" fhir-version)) "r5"]
                       [else #f]))
  (define dirs (append (current-package-directories)
                       (let ([core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t)])
                         (if core (let-values ([(d _n _m) (split-path core)]) (if (path? d) (list d) '())) '()))))
  (if (not suffix)
      '()
      (for*/list ([id (in-list (list (string-append "hl7.fhir.uv.extensions." suffix) (string-append "hl7.terminology." suffix)))]
                  [found (in-value (newest-package id dirs))] #:when found)
        found)))
;; newest-package : id dirs -> tgz-path|#f   <id>-<version>.tgz with the highest version in the dirs
(define (newest-package id dirs)
  (define rx (pregexp (string-append "^" (regexp-quote id) "-([0-9][^/]*)[.]tgz$")))
  (define found
    (for*/list ([d (in-list dirs)] #:when (directory-exists? d)
                [f (in-list (directory-list d))]
                [m (in-value (regexp-match rx (path->string f)))] #:when m)
      (cons (cadr m) (build-path d f))))
  (and (pair? found) (cdr (car (sort found version>? #:key car)))))
(define (version>? a b)
  (let loop ([as (string-split a ".")] [bs (string-split b ".")])
    (cond [(null? as) #f] [(null? bs) #t]
          [else (define x (string->number (car as))) (define y (string->number (car bs)))
                (cond [(and x y (not (= x y))) (> x y)]
                      [(and (not (and x y)) (not (equal? (car as) (car bs)))) (string>? (car as) (car bs))]
                      [else (loop (cdr as) (cdr bs))])])))
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
  (require rackunit racket/path)
  (define f (make-temporary-file))
  (with-output-to-file f #:exists 'truncate
    (lambda () (display "canonical: http://hl7.org/fhir/us/mcode\nfhirVersion: 4.0.1\ndependencies:\n  hl7.fhir.uv.genomics-reporting: 2.0.0\n#  hl7.fhir.uv.extensions: 5.1.0-cibuild\n  hl7.fhir.us.core: \n    version: 6.1.0\n    id: hl7fhiruscore\nextension:\n  - url: x\n")))
  (define c (read-ig-config f))
  (check-equal? (ig-config-canonical c) "http://hl7.org/fhir/us/mcode")
  (check-equal? (ig-config-fhir-version c) "4.0.1")
  (check-equal? (ig-config-dependencies c) '(("hl7.fhir.uv.genomics-reporting" . "2.0.0") ("hl7.fhir.us.core" . "6.1.0")))
  ;; the implicit packages: the newest copy of each, none for an unknown version
  (check-false (version>? "5.2.0" "5.10.0"))
  (check-true (version>? "5.10.0" "5.2.0"))
  (check-equal? (implicit-packages "3.0.2") '())
  (let ([dir (make-temporary-file "implicit-~a" 'directory)])
    (for ([n '("hl7.fhir.uv.extensions.r4-1.0.0.tgz" "hl7.fhir.uv.extensions.r4-5.2.0.tgz" "other.tgz")])
      (with-output-to-file (build-path dir n) (lambda () (display "x"))))
    (parameterize ([current-package-directories (list dir)])
      (check-equal? (map (lambda (p) (path->string (file-name-from-path p))) (implicit-packages "4.0.1")) '("hl7.fhir.uv.extensions.r4-5.2.0.tgz")))
    (delete-directory/files dir))
  (define g (make-temporary-file))
  (with-output-to-file g #:exists 'truncate
    (lambda () (display "id: hl7.fhir.uv.ips\nversion: 2.0.1\ntitle: \"IPS\"\nlicense: CC0-1.0\npublisher:\n  name: HL7 International\n  url: http://hl7.org\n")))
  (define gc (read-ig-config g))
  (check-equal? (list (ig-config-id gc) (ig-config-version gc) (ig-config-title gc) (ig-config-publisher gc))
                '("hl7.fhir.uv.ips" "2.0.1" "IPS" "HL7 International")))
