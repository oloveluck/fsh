#lang racket/base

;;; ig-fetch.rkt — a published package as a test oracle, fetched from packages.fhir.org when it is not
;;; in fhir-spec: downloaded once into tests/vendor/packages/ (not committed), verified against a
;;; pinned sha1, and #f when the registry is unreachable (the test then skips with a note). CI has the
;;; network; a sandbox without it still runs everything else.

(require racket/contract racket/file racket/port racket/runtime-path net/url file/sha1
         (only-in fhir/spec-package spec-package-path))

(provide
 (contract-out
  [packages-dir   path?]
  [fetched-package (-> string? string? string? (or/c path? #f))]))

(define-runtime-path packages-dir "vendor/packages")
(define registry "https://packages.fhir.org")

;; fetched-package : id version sha1 -> path|#f   <id>-<version>.tgz from fhir-spec, the cache, or the
;; registry (whose shasum must match the pin); #f when it cannot be had
(define (fetched-package id version sha)
  (define name (format "~a-~a.tgz" id version))
  (define cached (build-path packages-dir name))
  (cond
    [(spec-package-path name #:optional? #t)]
    [(file-exists? cached) cached]
    [else
     (define bytes
       (with-handlers ([exn:fail? (lambda (e) (eprintf "  ~a: not fetched (~a)\n" name (exn-message e)) #f)])
         (call/input-url (string->url (format "~a/~a/~a" registry id version)) get-pure-port port->bytes)))
     (cond
       [(not bytes) #f]
       [(not (equal? (sha1 (open-input-bytes bytes)) sha))
        (eprintf "  ~a: the registry's file has sha1 ~a, the test pins ~a\n" name (sha1 (open-input-bytes bytes)) sha) #f]
       [else (make-directory* packages-dir)
             (call-with-output-file cached (lambda (o) (write-bytes bytes o)))
             cached])]))

(module+ test
  (require rackunit)
  ;; a package fhir-spec holds needs no fetch
  (check-not-false (fetched-package "hl7.fhir.us.mcode" "4.0.0" "-"))
  (check-true (path? packages-dir)))
