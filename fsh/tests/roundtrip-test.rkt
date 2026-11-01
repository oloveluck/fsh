#lang racket/base

;;; roundtrip-test.rkt — bidirectionality: FSH -> FHIR -> FSH -> FHIR is a fixpoint.
;;;
;;; For every vendored golden `.fsh`, compile to FHIR₁, decompile FHIR₁ back to FSH text, recompile
;;; that to FHIR₂, and require FHIR₁ ≡ FHIR₂ (deep-equal after canonicalising hashes/lists). This
;;; proves the forward (compile) and reverse (decompile) directions round-trip losslessly on the FHIR
;;; side. Also checks a parse∘print∘parse AST fixpoint via the printer.

(require rackunit
         racket/runtime-path
         racket/list
         (only-in racket/port port->string)
         (only-in fhir/r5/schemas)                      ; register the R5 tower
         (only-in fhir/fsh/compile fsh->fhir canonical-base)
         (only-in fhir/fsh/decompile fhir->fsh)
         (only-in fhir/fsh/parser/parse parse-fsh-doc)
         (only-in fhir/fsh/print print-doc))

(define-runtime-path golden-dir "vendor/golden")

;; deep structural equality for jsexpr (hash key order independent; lists compared in order)
(define (jequal? a b)
  (cond
    [(and (hash? a) (hash? b))
     (and (= (hash-count a) (hash-count b))
          (for/and ([(k v) (in-hash a)]) (and (hash-has-key? b k) (jequal? v (hash-ref b k)))))]
    [(and (list? a) (list? b)) (and (= (length a) (length b)) (andmap jequal? a b))]
    [else (equal? a b)]))

;; every vendored golden now round-trips (forward compile + decompile are mutually inverse on FHIR)
(define reverse-deferred '())

(define (fsh-files)
  (sort (filter (lambda (p) (and (regexp-match? #rx"[.]fsh$" (path->string p))
                                 (not (member (regexp-replace #rx"[.]fsh$" (path->string p) "")
                                              reverse-deferred))))
                (directory-list golden-dir))
        string<? #:key path->string))

(define (run)
  (parameterize ([canonical-base "http://example.org"])
    (define pass 0) (define fail 0)
    (for ([f (in-list (fsh-files))])
      (define name (regexp-replace #rx"[.]fsh$" (path->string f) ""))
      (define src (call-with-input-file (build-path golden-dir f) port->string))
      (define fhir1 (with-handlers ([exn:fail? (lambda (e) (list (hasheq 'error (exn-message e))))]) (fsh->fhir src)))
      (define fsh2 (apply fhir->fsh fhir1))
      (define fhir2 (with-handlers ([exn:fail? (lambda (e) (list (hasheq 'error (exn-message e))))]) (fsh->fhir fsh2)))
      (cond
        [(and (= (length fhir1) (length fhir2)) (andmap jequal? fhir1 fhir2)) (set! pass (add1 pass))]
        [else (set! fail (add1 fail))
              (printf "ROUNDTRIP FAIL ~a\n  FSH2=\n~a\n  fhir1=~s\n  fhir2=~s\n" name fsh2 fhir1 fhir2)]))
    (printf "\nFSH round-trip (FSH->FHIR->FSH->FHIR): ~a/~a fixpoint\n" pass (+ pass fail))
    (values pass fail)))

(module+ test
  ;; AST fixpoint: parse = parse∘print∘parse (the printer is a faithful inverse of the parser)
  (for ([f (in-list (fsh-files))])
    (define src (call-with-input-file (build-path golden-dir f) port->string))
    (define ast1 (parse-fsh-doc src))
    (define ast2 (parse-fsh-doc (print-doc ast1)))
    (check-equal? ast2 ast1 (format "print fixpoint: ~a" f)))
  (define-values (pass fail) (run))
  (check-equal? fail 0 (format "~a round-trip case(s) failed" fail)))

(module+ main (run))
