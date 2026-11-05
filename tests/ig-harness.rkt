#lang racket/base

;;; ig-harness.rkt — compile a real FSH-authored Implementation Guide and compare the result with the
;;; IG's PUBLISHED package (what SUSHI + the IG Publisher produced from the same sources).
;;;
;;; Resources are paired by (resourceType, id). Before comparing, both sides are normalised to drop
;;; what the IG Publisher (not SUSHI) adds or what comes from sushi-config.yaml, which this compiler
;;; does not read:
;;;   top level   text, snapshot, date, version, publisher, contact, jurisdiction; meta except profile;
;;;               publisher-assigned OID identifiers on conformance resources
;;;   anywhere    structuredefinition-wg / -fmm / -standards-status / -normative-version extensions;
;;;               a `|<version>` pin on hl7.org/fhir canonicals; xml:lang/lang on narrative divs, their
;;;               whitespace and <x></x> vs <x/>; the `text` of a nested resource (Bundle entries: generated
;;;               narratives); a bare `{id, path}` differential element (the publisher inserts a root
;;;               or an intermediate element on some profiles, by processing order);
;;;               a compose component `version` the publisher pinned where ours has none
;;; Measures (each ratcheted by the caller):
;;;   exact      — normalised published = normalised ours
;;;   contained  — every published value is present in ours (lists positional)
;;;   elements   — StructureDefinition differential elements (by id) whose published content is
;;;                fully present in ours
;;;   errors     — entities that failed to compile; unmatched — pairing failures both ways
;;;
;;; Data: (ig-fsh-report pairs exact contained elements-ok elements-total errors ours-unmatched
;;;                      published-unmatched diffs classes)
;;;   diffs   : (listof (list key first-difference))
;;;   classes : (listof (list class count examples)) — every published value absent or different in
;;;             ours, grouped by WHAT differs (kind + path with indices and ids generalised, e.g.
;;;             "missing differential.element[].short"), largest class first, with up to three
;;;             (key published ours) examples each: the work list for fidelity to SUSHI

(require racket/contract racket/list racket/string racket/file racket/match
         json file/gunzip file/untar
         fsh/compile
         (only-in fsh/parser/parse parse-fsh-doc))

(provide
 (struct-out ig-fsh-report)
 (contract-out
  [check-fsh-ig (->* (path-string? path-string? string?) (#:fhir-version string? #:dump (listof string?)) ig-fsh-report?)]))

(struct ig-fsh-report (pairs exact contained elements-ok elements-total errors ours-unmatched
                             published-unmatched diffs classes)
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

;; the IG Publisher assigns conformance resources an OID identifier (urn:ietf:rfc:3986 urn:oid:…)
;; from its oids.ini — not SUSHI output
(define (publisher-oid? i) (and (hash? i) (equal? (hash-ref i 'system #f) "urn:ietf:rfc:3986")
                                (regexp-match? #rx"^urn:oid:" (hash-ref i 'value ""))))
(define conformance-types '("StructureDefinition" "ValueSet" "CodeSystem" "CapabilityStatement" "OperationDefinition"
                            "SearchParameter" "ConceptMap" "NamingSystem" "ImplementationGuide"))
(define (drop-publisher-oids j)
  (define ids (and (hash? j) (member (hash-ref j 'resourceType #f) conformance-types) (hash-ref j 'identifier #f)))
  (cond [(list? ids)
         (define kept (filter (lambda (i) (not (publisher-oid? i))) ids))
         (if (null? kept) (hash-remove j 'identifier) (hash-set j 'identifier kept))]
        [(and (hash? ids) (publisher-oid? ids)) (hash-remove j 'identifier)]   ; R4's ConceptMap.identifier is 0..1
        [else j]))

;; the IG Publisher inserts a bare `{id, path}` root (and intermediate) element into a differential
;; whose first element is below the root — on some profiles, by processing order. It says nothing;
;; dropped on both sides. (SUSHI never writes one for a profile.)
(define (content-free? e) (and (hash? e) (andmap (lambda (k) (memq k '(id path))) (hash-keys e))))
(define (drop-content-free-elements j)
  (define d (and (hash? j) (hash-ref j 'differential #f)))
  (if (and (hash? d) (list? (hash-ref d 'element #f)))
      (hash-set j 'differential (hash-set d 'element (filter (lambda (e) (not (content-free? e))) (hash-ref d 'element))))
      j))
;; narratives are publisher output: a resource's `text` (Bundle entries too) is dropped, and the
;; XHTML of a hand-written one (Composition sections) is compared with its whitespace collapsed
(define (collapse-xhtml s0)
  (define s (regexp-replace* #px"(\\w+)='([^']*)'" s0 "\\1=\"\\2\""))   ; attributes in double quotes
  (regexp-replace* #px"<(\\w+)></\\1>"                       ; the publisher writes an empty element <span/>
                   (string-trim (regexp-replace* #px">\\s+<" (regexp-replace* #px"\\s+" s " ") "><"))
                   "<\\1/>"))

(define (norm j0 #:top? [top? #t])
  (define j (if top? (drop-content-free-elements (drop-publisher-oids j0)) j0))
  (cond
    [(hash? j)
     (for/hash ([(k v) (in-hash j)]
                #:unless (and top? (memq k drop-top))
                #:unless (and (not top?) (eq? k 'text) (hash-ref j 'resourceType #f))
                #:unless (and top? (eq? k 'meta) (not (hash-ref v 'profile #f)))
                #:unless (and (eq? k 'extension) (list? v) (andmap publisher-ext? v)))
       (values k (cond [(and top? (eq? k 'meta)) (hasheq 'profile (norm (hash-ref v 'profile) #:top? #f))]
                       [(and (eq? k 'extension) (list? v)) (norm (filter (lambda (e) (not (publisher-ext? e))) v) #:top? #f)]
                       [else (norm v #:top? #f)])))]
    [(list? j) (map (lambda (x) (norm x #:top? #f)) j)]
    [(real? j) (exact->inexact j)]
    [(and (string? j) (regexp-match? #rx"^http://hl7[.]org/fhir/.*[|][0-9][^|/]*$" j)) (car (string-split j "|"))]
    [(and (string? j) (regexp-match? #rx"^<div " j))
     (collapse-xhtml (regexp-replace* #rx" (xml:lang|lang)=\"[^\"]*\"" (regexp-replace* #px"(\\w+)='([^']*)'" j "\\1=\"\\2\"") ""))]
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

;; all-diffs : published ours path -> (listof Diff)   every published value absent or different in
;; ours; lists of elements with ids pair BY ID (snapshot order is SUSHI's business, the content ours)
;; A Diff is (list 'missing path value) | (list 'differs path published ours) | (list 'short-list path n m)
(define (all-diffs a b path)
  (cond
    [(and (hash? a) (hash? b))
     (append* (for/list ([(k v) (in-hash a)])
                (if (hash-has-key? b k)
                    (all-diffs v (hash-ref b k) (cons k path))
                    (list (list 'missing (reverse (cons k path)) v)))))]
    [(and (list? a) (list? b) (ids? a) (ids? b))
     (define ours (for/hash ([e (in-list b)]) (values (hash-ref e 'id) e)))
     (append* (for/list ([e (in-list a)])
                (define o (hash-ref ours (hash-ref e 'id) #f))
                (if o (all-diffs e o (cons (hash-ref e 'id) path))
                    (list (list 'missing (reverse (cons (hash-ref e 'id) path)) e)))))]
    [(and (list? a) (list? b))
     (append (if (> (length a) (length b)) (list (list 'short-list (reverse path) (length a) (length b))) '())
             (append* (for/list ([x a] [y b] [i (in-naturals)]) (all-diffs x y (cons i path)))))]
    [(equal? a b) '()]
    [else (list (list 'differs (reverse path) a b))]))
(define (ids? l) (and (pair? l) (andmap (lambda (e) (and (hash? e) (string? (hash-ref e 'id #f)))) l)))

;; diff-class : Diff -> String   the kind and the path with indices and element ids generalised
(define (diff-class d)
  (string-append (symbol->string (car d)) " "
                 (string-join (for/list ([seg (in-list (cadr d))])
                                (cond [(symbol? seg) (symbol->string seg)]
                                      [(number? seg) "[]"]
                                      [else "<id>"]))
                              ".")))
;; classes-of : (listof (cons key Diff)) -> (listof (list class count examples))   largest first
(define (classes-of keyed)
  (define groups (for/fold ([h (hash)]) ([kd (in-list keyed)])
                   (hash-update h (diff-class (cdr kd)) (lambda (l) (cons kd l)) '())))
  (sort (for/list ([(class kds) (in-hash groups)])
          (list class (length kds)
                (for/list ([kd (in-list (take (reverse kds) (min 3 (length kds))))])
                  (list (car kd) (cddr (cdr kd))))     ; (key (published ours)) or (key (value))
                (length (remove-duplicates (map car kds)))))   ; how many resources the class touches
        > #:key cadr))

(define (first-diff a b path)
  (cond [(and (hash? a) (hash? b))
         (for/or ([(k v) (in-hash a)])
           (if (hash-has-key? b k) (first-diff v (hash-ref b k) (cons k path)) (list 'missing (reverse (cons k path)))))]
        [(and (list? a) (list? b))
         (if (> (length a) (length b)) (list 'short-list (reverse path) (length a) (length b))
             (for/or ([x a] [y b] [i (in-naturals)]) (first-diff x y (cons i path))))]
        [(equal? a b) #f]
        [else (list 'differs (reverse path) a b)]))

(define (check-fsh-ig fsh-dir published-tgz canonical #:fhir-version [fv "4.0.1"] #:dump [dump '()])
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
  ;; #:dump: the differential elements (id min max) of the named resources, published then ours
  (for ([p (in-list pairs)] #:when (member (cdr (car p)) dump))
    ;; (an SD's own extensions first: a logical model's characteristics)
    (define (els sd) (cons (hash-ref sd 'extension '-)
                           (for/list ([e (in-list (hash-ref (hash-ref sd 'differential (hash)) 'element '()))])
                             (list (hash-ref e 'id #f) (hash-ref e 'min '-) (hash-ref e 'max '-)))))
    (if (hash-ref (cadr p) 'differential #f)
        (printf "  dump ~a published: ~s\n  dump ~a ours: ~s\n" (cdr (car p)) (els (cadr p)) (cdr (car p)) (els (caddr p)))
        (printf "  dump ~a published: ~a\n  dump ~a ours: ~a\n" (cdr (car p)) (jsexpr->string (cadr p)) (cdr (car p)) (jsexpr->string (caddr p)))))
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
                   (list (car p) (first-diff (cadr p) (caddr p) '())))
                 (classes-of (for*/list ([p (in-list pairs)] [d (in-list (all-diffs (cadr p) (caddr p) '()))])
                               (cons (car p) d)))))

;; print-classes : ig-fsh-report [n] -> void   the work list, largest class first, with examples
(provide print-classes)
(define (print-classes r [n 25])
  (define (short v) (let ([s (format "~s" v)]) (if (> (string-length s) 100) (string-append (substring s 0 100) "…") s)))
  (for ([c (in-list (take (ig-fsh-report-classes r) (min n (length (ig-fsh-report-classes r)))))])
    (printf "    ~a  ~a~a\n" (cadr c) (car c) (if (> (cadddr c) 1) (format "  (~a resources)" (cadddr c)) ""))
    (for ([ex (in-list (caddr c))])
      (printf "        ~a/~a: ~a\n" (car (car ex)) (cdr (car ex)) (string-join (map short (cadr ex)) "  |  ")))))
