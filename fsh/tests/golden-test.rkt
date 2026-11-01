#lang racket/base

;;; golden-test.rkt — FSH conformance against the SUSHI golden corpus.
;;;
;;; For each vendored `<name>.fsh` + `<name>.json` pair (compiled by the reference tool SUSHI at
;;; canonical http://example.org), compile the FSH and check the expected JSON is a SUBSET of our
;;; output — a tolerant match (mirroring fhir4s's JsonSubset): every scalar/field SUSHI emits must be
;;; present and equal in ours; ours may carry extras (e.g. unchanged cardinality bounds, extra
;;; elements). List items match positionally-independently (each expected item must match some actual
;;; item), so differential `element` arrays line up by content (path/sliceName) not order.

(require rackunit
         racket/runtime-path
         racket/list
         json
         (only-in racket/port port->string)
         (only-in fhir/r5/schemas)                 ; register the R5 tower (resolve.rkt reads it)
         (only-in fhir/fsh/compile fsh->fhir canonical-base))

(define-runtime-path golden-dir "vendor/golden")

;; expected ⊆ actual : deep tolerant subset
(define (subset? exp act)
  (cond
    [(hash? exp) (and (hash? act)
                      (for/and ([(k v) (in-hash exp)])
                        (and (hash-has-key? act k) (subset? v (hash-ref act k)))))]
    [(list? exp) (and (list? act) (for/and ([e (in-list exp)]) (for/or ([a (in-list act)]) (subset? e a))))]
    [else (equal? exp act)]))

;; pick the compiled resource matching the golden's resourceType + id
(define (pick compiled expected)
  (or (for/or ([r (in-list compiled)]
               #:when (and (equal? (hash-ref r 'resourceType #f) (hash-ref expected 'resourceType #f))
                           (equal? (hash-ref r 'id #f) (hash-ref expected 'id #f))))
        r)
      (and (pair? compiled) (car compiled))))

(define (fsh-cases)
  (sort (filter (lambda (p) (regexp-match? #rx"[.]fsh$" (path->string p)))
                (directory-list golden-dir))
        string<? #:key path->string))

(define (run)
  (parameterize ([canonical-base "http://example.org"])
    (define pass 0) (define fail 0)
    (for ([f (in-list (fsh-cases))])
      (define name (regexp-replace #rx"[.]fsh$" (path->string f) ""))
      (define jf (build-path golden-dir (string-append name ".json")))
      (when (file-exists? jf)
        (define expected (call-with-input-file jf read-json))
        (define src (call-with-input-file (build-path golden-dir f) port->string))
        (define compiled (with-handlers ([exn:fail? (lambda (e) (list (hasheq 'error (exn-message e))))])
                           (fsh->fhir src)))
        (define actual (pick compiled expected))
        (cond
          [(and actual (subset? expected actual)) (set! pass (add1 pass))]
          [else (set! fail (add1 fail))
                (printf "FAIL ~a\n  expected ⊄ actual\n  actual=~s\n" name actual)])))
    (printf "\nFSH golden: ~a/~a passed\n" pass (+ pass fail))
    (values pass fail)))

(module+ test
  (define-values (pass fail) (run))
  (check-equal? fail 0 (format "~a golden case(s) failed" fail)))

(module+ main (run))
