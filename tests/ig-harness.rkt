#lang racket/base

;;; ig-harness.rkt — compile a real FSH-authored Implementation Guide and compare the result with the
;;; IG's PUBLISHED package (what SUSHI + the IG Publisher produced from the same sources).
;;;
;;; Resources are paired by (resourceType, id). Before comparing, both sides are normalised to drop
;;; what the IG Publisher (not SUSHI) adds or what comes from sushi-config.yaml, which this compiler
;;; does not read:
;;;   top level   text, snapshot, date, version, publisher, contact, jurisdiction; meta except profile
;;;   anywhere    structuredefinition-wg / -fmm / -standards-status / -normative-version extensions;
;;;               a `|<version>` pin on hl7.org/fhir canonicals; xml:lang/lang on narrative divs;
;;;               a compose component `version` the publisher pinned where ours has none
;;; Measures (each ratcheted by the caller):
;;;   exact      — normalised published = normalised ours
;;;   contained  — every published value is present in ours (lists positional)
;;;   elements   — StructureDefinition differential elements (by id) whose published content is
;;;                fully present in ours
;;;   errors     — entities that failed to compile; unmatched — pairing failures both ways
;;;
;;; Data: (ig-fsh-report pairs exact contained elements-ok elements-total errors ours-unmatched
;;;                      published-unmatched diffs)   diffs : (listof (list key first-difference))

(require racket/contract racket/list racket/string racket/file racket/match
         json file/gunzip file/untar
         fsh/compile
         (only-in fsh/parser/parse parse-fsh-doc))

(provide
 (struct-out ig-fsh-report)
 (contract-out
  [check-fsh-ig (->* (path-string? path-string? string?) (#:fhir-version string?) ig-fsh-report?)]))

(struct ig-fsh-report (pairs exact contained elements-ok elements-total errors ours-unmatched
                             published-unmatched diffs)
  #:transparent)

;; published package resources keyed by (type . id)
(define (published tgz)
  (define pub (make-hash))
  (define-values (pin pout) (make-pipe (* 1024 1024)))
  (define t (thread (lambda () (call-with-input-file tgz (lambda (gz) (gunzip-through-ports gz pout)))
                      (close-output-port pout))))
  (untar pin #:handle-entry
         (lambda (k n c sz a)
           (when (eq? k 'file)
             (define bs (read-bytes sz c))
             (when (regexp-match? #px"^package/(example/)?[^./][^/]*\\.json$" (path->string n))
               (define j (with-handlers ([exn:fail? (lambda _ #f)]) (bytes->jsexpr bs)))
               (when (and (hash? j) (hash-ref j 'id #f))
                 (hash-set! pub (cons (hash-ref j 'resourceType) (hash-ref j 'id)) j))))
           '()))
  (thread-wait t)
  pub)

(define publisher-ext #rx"structuredefinition-(wg|fmm|standards-status|normative-version)")
(define drop-top '(text snapshot date version publisher contact jurisdiction))
(define (publisher-ext? e) (and (hash? e) (regexp-match? publisher-ext (hash-ref e 'url ""))))

(define (norm j #:top? [top? #t])
  (cond
    [(hash? j)
     (for/hash ([(k v) (in-hash j)]
                #:unless (and top? (memq k drop-top))
                #:unless (and top? (eq? k 'meta) (not (hash-ref v 'profile #f)))
                #:unless (and (eq? k 'extension) (list? v) (andmap publisher-ext? v)))
       (values k (cond [(and top? (eq? k 'meta)) (hasheq 'profile (norm (hash-ref v 'profile) #:top? #f))]
                       [(and (eq? k 'extension) (list? v)) (norm (filter (lambda (e) (not (publisher-ext? e))) v) #:top? #f)]
                       [else (norm v #:top? #f)])))]
    [(list? j) (map (lambda (x) (norm x #:top? #f)) j)]
    [(real? j) (exact->inexact j)]
    [(and (string? j) (regexp-match? #rx"^http://hl7[.]org/fhir/.*[|][0-9][^|/]*$" j)) (car (string-split j "|"))]
    [(and (string? j) (regexp-match? #rx"^<div " j)) (regexp-replace* #rx" (xml:lang|lang)=\"[^\"]*\"" j "")]
    [else j]))

;; the publisher pins versions on compose components; an unpinned component of ours still matches
(define (unpin-versions p o)
  (define (comps j k) (let ([c (and (hash? j) (hash-ref j 'compose #f))]) (if (hash? c) (hash-ref c k '()) '())))
  (cond
    [(and (hash? p) (hash? (hash-ref p 'compose #f)))
     (hash-set p 'compose
               (for/fold ([c (hash-ref p 'compose)]) ([k '(include exclude)] #:when (hash-has-key? (hash-ref p 'compose) k))
                 (hash-set c k (for/list ([pc (in-list (hash-ref c k))] [i (in-naturals)])
                                 (define oc (let ([l (comps o k)]) (and (< i (length l)) (list-ref l i))))
                                 (if (and (hash? oc) (hash? pc) (not (hash-has-key? oc 'version))) (hash-remove pc 'version) pc)))))]
    [else p]))

(define (subset? a b)
  (cond [(and (hash? a) (hash? b)) (for/and ([(k v) (in-hash a)]) (and (hash-has-key? b k) (subset? v (hash-ref b k))))]
        [(and (list? a) (list? b)) (and (<= (length a) (length b)) (for/and ([x a] [y b]) (subset? x y)))]
        [else (equal? a b)]))

(define (first-diff a b path)
  (cond [(and (hash? a) (hash? b))
         (for/or ([(k v) (in-hash a)])
           (if (hash-has-key? b k) (first-diff v (hash-ref b k) (cons k path)) (list 'missing (reverse (cons k path)))))]
        [(and (list? a) (list? b))
         (if (> (length a) (length b)) (list 'short-list (reverse path) (length a) (length b))
             (for/or ([x a] [y b] [i (in-naturals)]) (first-diff x y (cons i path))))]
        [(equal? a b) #f]
        [else (list 'differs (reverse path) a b)]))

(define (check-fsh-ig fsh-dir published-tgz canonical #:fhir-version [fv "4.0.1"])
  (define files (sort (for/list ([f (in-directory fsh-dir)] #:when (regexp-match? #rx"[.]fsh$" (path->string f))) f)
                      string<? #:key path->string))
  (define-values (out errors)
    (parameterize ([canonical-base canonical] [fsh-fhir-version fv])
      (compile-doc/errors (parse-fsh-doc (string-join (map file->string files) "\n")))))
  (define pub (published published-tgz))
  (define ours (for/hash ([o (in-list out)] #:when (hash-ref o 'id #f))
                 (values (cons (hash-ref o 'resourceType) (hash-ref o 'id)) o)))
  (define pairs (for/list ([(k o) (in-hash ours)] #:when (hash-ref pub k #f))
                  (define no (norm o))
                  (list k (unpin-versions (norm (hash-ref pub k)) no) no)))
  (define-values (el-ok el-total)
    (for*/fold ([ok 0] [t 0]) ([p (in-list pairs)] #:when (equal? (car (car p)) "StructureDefinition"))
      (define (by-id sd) (for/hash ([e (in-list (hash-ref (hash-ref sd 'differential (hash)) 'element '()))])
                           (values (hash-ref e 'id #f) e)))
      (define po (by-id (cadr p))) (define oo (by-id (caddr p)))
      (for/fold ([ok ok] [t t]) ([(id e) (in-hash po)])
        (values (+ ok (if (and (hash-ref oo id #f) (subset? e (hash-ref oo id))) 1 0)) (add1 t)))))
  (ig-fsh-report (length pairs)
                 (count (lambda (p) (equal? (cadr p) (caddr p))) pairs)
                 (count (lambda (p) (subset? (cadr p) (caddr p))) pairs)
                 el-ok el-total
                 (map (lambda (e) (cons (car e) (exn-message (cdr e)))) errors)
                 (sort (for/list ([k (in-hash-keys ours)] #:unless (hash-ref pub k #f)) k) string<? #:key car)
                 (sort (for/list ([k (in-hash-keys pub)] #:unless (hash-ref ours k #f)) k) string<? #:key car)
                 (for/list ([p (in-list pairs)] #:unless (subset? (cadr p) (caddr p)))
                   (list (car p) (first-diff (cadr p) (caddr p) '())))))
