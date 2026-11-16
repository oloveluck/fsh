#lang racket/base

;;; instance-of-test.rkt — `fhir->fsh/of-profile` over published examples.
;;;
;;; Every fourth example of IPS 2.0.1 and mCODE 4.0.0 (Bundles aside), written as an instance of the
;;; profile it claims, in the package context their own FSH compiles in. The law: the text compiles
;;; back to exactly the resource. Ratcheted: how many are stated as instances of their profile, and
;;; how much shorter the texts are than `fhir->fsh`'s (floors that only rise).

(require rackunit racket/list racket/string
         (only-in fhir/r4/schemas)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/package read-package)
         fsh/decompile fsh/instance-of
         (only-in fsh/compile fsh->fhir fsh-fhir-version))

(define ips (spec-package-path "hl7.fhir.uv.ips.tgz" #:optional? #t))
(define mcode (spec-package-path "hl7.fhir.us.mcode-4.0.0.tgz" #:optional? #t))
(define us-core (spec-package-path "hl7.fhir.us.core-6.1.0.tgz" #:optional? #t))
(define core (spec-package-path "hl7.fhir.r4.core.tgz" #:optional? #t))

(define (comparable j)
  (cond [(hash? j) (for/hasheq ([(k v) (in-hash j)]) (values k (comparable v)))]
        [(list? j) (map comparable j)]
        [(real? j) (inexact->exact j)]
        [else j]))

(cond
  [(not (and ips mcode us-core core))
   (printf "  instance-of: skipped (needs the fhir-spec packages)\n")]
  [else
   (define examples
     (for*/list ([tgz (in-list (list ips mcode))]
                 [e (in-list (let-values ([(_m _r examples) (read-package tgz)]) examples))]
                 #:unless (member (hash-ref (cdr e) 'resourceType #f) '("Bundle" "ImplementationGuide")))
       (hash-remove (cdr e) 'text)))
   (define sampled
     (for/list ([j (in-list (sort (remove-duplicates examples #:key (lambda (j) (hash-ref j 'id ""))) string<?
                                  #:key (lambda (j) (hash-ref j 'id ""))))]
                [i (in-naturals)] #:when (zero? (modulo i 4)))
       j))
   (parameterize ([current-sd-packages (list ips mcode us-core core)] [fsh-fhir-version "4.0.1"])
     (define rows
       (for/list ([j (in-list sampled)])
         (define plain (fhir->fsh j))
         (define own (fhir->fsh/of-profile j))
         (define back (findf (lambda (r) (equal? (hash-ref r 'id #f) (hash-ref j 'id #f)))
                             (with-handlers ([exn:fail? (lambda (_) '())]) (fsh->fhir own))))
         ;; the law, wherever the text was changed (an unchanged text is fhir->fsh's, held elsewhere)
         (unless (equal? own plain)
           (check-equal? (and back (comparable back)) (comparable j) (hash-ref j 'id)))
         (list (hash-ref j 'id) (string-length plain) (string-length own) (not (equal? own plain))
               (regexp-match? #rx"\\[[a-zA-Z][^]]*\\]" (string-join (filter (lambda (l) (regexp-match? #rx"^[*] " l)) (string-split own "\n")) "\n")))))
     (define of-profile (count fourth rows))
     (define plain-chars (apply + (map second rows)))
     (define own-chars (apply + (map third rows)))
     (printf "  instance-of: ~a examples; ~a stated as instances of their profile (~a with an entry by its slice); ~a -> ~a characters (~a%)\n"
             (length rows) of-profile (count fifth rows) plain-chars own-chars (round (* 100.0 (/ own-chars plain-chars))))
     (check-true (>= of-profile 48) (format "~a stated as instances of their profile" of-profile))
     (check-true (<= (* 100 own-chars) (* 84 plain-chars)) (format "~a of ~a characters" own-chars plain-chars)))])
