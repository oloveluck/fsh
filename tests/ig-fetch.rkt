#lang racket/base

;;; ig-fetch.rkt — a published package as a test oracle, fetched from packages.fhir.org when it is not
;;; in fhir-spec: downloaded once into tests/vendor/packages/ (not committed), verified against a
;;; pinned sha1, and #f when the registry is unreachable (the test then skips with a note). CI has the
;;; network; a sandbox without it still runs everything else.

(require racket/contract racket/file racket/port racket/runtime-path net/url file/sha1 json
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/package read-package))

(provide
 (contract-out
  [packages-dir   path?]
  ;; fetched-package : id version [sha1] -> path|#f   <id>-<version>.tgz from fhir-spec, the cache, or
  ;; the registry — verified against the pinned sha1 (or, with none, the registry's own shasum, printed
  ;; so the test can pin it); #f when it cannot be had. A version-alias STUB (hl7.fhir.us.core.v311: a
  ;; package.json that depends on hl7.fhir.us.core 3.1.1 and nothing else) is followed to the package
  ;; it names, as SUSHI's loader does.
  [fetched-package (->* (string? string?) ((or/c string? #f)) (or/c path? #f))]
  ;; stub-dependency : tgz -> (cons id version)|#f   the one package a version-alias stub stands for
  [stub-dependency (-> path? (or/c (cons/c string? string?) #f))]))

(define-runtime-path packages-dir "vendor/packages")
(define registry "https://packages.fhir.org")

(define (fetched-package id version [sha #f])
  (define name (format "~a-~a.tgz" id version))
  (define cached (build-path packages-dir name))
  (define file
    (cond
      [(spec-package-path name #:optional? #t)]
      [(file-exists? cached) cached]
      [else (download! id version sha cached)]))
  (cond [(not file) #f]
        [(stub-dependency file) => (lambda (d) (fetched-package (car d) (cdr d)))]
        [else file]))

;; download! : id version sha1|#f path -> path|#f
(define (download! id version sha cached)
  (define name (format "~a-~a.tgz" id version))
  (define (fetch url) (call/input-url (string->url url) get-pure-port port->bytes))
  (with-handlers ([exn:fail? (lambda (e) (eprintf "  ~a: not fetched (~a)\n" name (exn-message e)) #f)])
    (define bytes (fetch (format "~a/~a/~a" registry id version)))
    (define sum (sha1 (open-input-bytes bytes)))
    (define expected
      (or sha
          (let ([entry (bytes->jsexpr (fetch (format "~a/~a" registry id)))])
            (hash-ref (hash-ref (hash-ref (hash-ref entry 'versions) (string->symbol version)) 'dist) 'shasum))))
    (cond
      [(not (equal? sum expected))
       (eprintf "  ~a: the registry's file has sha1 ~a, ~a ~a\n" name sum (if sha "the test pins" "the registry says") expected) #f]
      [else (unless sha (printf "  ~a: fetched, sha1 ~a (pin it)\n" name sum))
            (make-directory* packages-dir)
            (call-with-output-file cached (lambda (o) (write-bytes bytes o)))
            cached])))

(define (stub-dependency tgz)
  (define-values (m _r _e) (read-package tgz #:select (lambda (_) #f)))
  (define deps (and (hash? m) (hash-ref m 'dependencies #f)))
  (define name (and (hash? m) (hash-ref m 'name #f)))
  (and (string? name) (regexp-match? #px"[.]v[0-9]+$" name)
       (hash? deps) (= 1 (hash-count deps))
       (for/first ([(k v) (in-hash deps)]) (cons (symbol->string k) v))))

(module+ test
  (require rackunit)
  ;; a package fhir-spec holds needs no fetch
  (check-not-false (fetched-package "hl7.fhir.us.mcode" "4.0.0" "-"))
  (check-false (stub-dependency (fetched-package "hl7.fhir.us.mcode" "4.0.0" "-")))   ; a real package
  (check-true (path? packages-dir)))
