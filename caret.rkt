#lang racket/base

;;; caret.rkt — get/set a value at an FSH caret path within a jsexpr.
;;;
;;; The single source of truth for caret-path semantics, shared by the forward compiler
;;; (apply `^prop = value`) and the reverse decompiler (emit a caret rule for any property a
;;; specialized rule doesn't cover). This is what makes the FHIR<->FSH round-trip lossless
;;; without snapshot generation. Array positions are written explicitly as `[i]` in the caret
;;; path (the decompiler emits indices it reads from the real jsexpr), so building is
;;; unambiguous: a bracket index ⇒ array; a plain segment ⇒ object key.

(require racket/match
         (only-in racket/list make-list list-set index-where)
         racket/contract
         "parser/path.rkt"
         (only-in "resolve.rkt" field-info)
         (only-in fhir/model/schema ftype-tag ftype-type-name))

(provide
 (contract-out
  ;; jsexpr, "^a.b[0].c", value -> updated jsexpr. Structural by default (an index makes an array,
  ;; a plain segment an object key — the decompiler's exact form). With #:type (the jsexpr's FHIR
  ;; type, e.g. 'StructureDefinition / 'ElementDefinition) it follows the schema, as SUSHI does:
  ;;   a repeating element without an index is element 0; `[+]`/`[=]` use #:counters (one mutable
  ;;   hash per entity, keyed by path prefix); `extension[url]` / `extension[sliceName]` select —
  ;;   or create, url set — the extension with that url; the value is shaped by #:coerce
  ;;   (value ftype -> jsexpr) for the target element's type.
  [caret-set   (->* (any/c string? any/c)
                    (#:type (or/c symbol? #f) #:counters (or/c hash? #f)
                     #:coerce (or/c procedure? #f) #:scope string?)
                    any/c)]
  [caret-ref   (-> any/c string? any/c)]         ; jsexpr, caret path -> value or #f
  [caret-steps (-> string? list?)]))             ; caret path string -> step list

;; the steps of a caret path string ("^slicing.discriminator[0].type" -> list of fsh-step)
(define (caret-steps caret-str) (fsh-caret-steps (parse-fsh-caret caret-str)))

(define (step-key s) (string->symbol (fsh-step-name s)))
(define (step-index s)
  (for/or ([b (in-list (fsh-step-brackets s))])
    (and (eq? (fsh-bracket-kind b) 'index) (fsh-bracket-value b))))

(define (ensure-hash x) (if (hash? x) x (hasheq)))

;; array-ref-or : any index -> element | #f   (non-list or out-of-range -> #f)
(define (array-ref-or lst i)
  (if (and (list? lst) (< i (length lst))) (list-ref lst i) #f))

;; array-set-pad : any index value -> list   set index i, padding a short/absent list with #f
(define (array-set-pad lst i v)
  (define xs (if (list? lst) lst '()))
  (define padded (append xs (make-list (max 0 (- (add1 i) (length xs))) #f)))
  (list-set padded i v))

;; set `value` at the caret path in `js`, building intermediate objects/arrays
(define (caret-set js caret-str value #:type [type #f] #:counters [counters #f] #:coerce [coerce #f]
                   #:scope [scope ""])
  (if type
      (set-typed (ensure-hash js) type (caret-steps caret-str) value (or counters (make-hash)) coerce scope)
      (set-steps (ensure-hash js) (caret-steps caret-str) value)))

;; the schema-following setter
(define (set-typed js type steps value counters coerce prefix)
  (define s (car steps)) (define more (cdr steps))
  (define key (step-key s))
  ;; the inherited framework fields are not in a type's own field specs: their shapes are fixed
  (define-values (kind ftype)
    (case key
      [(extension modifierExtension) (values 'list (vector 'type 'Extension))]
      [(id) (values 'optional (vector 'prim "string"))]
      [else (field-info type key)]))
  (define next-type (and (eq? (ftype-tag ftype) 'type) (ftype-type-name ftype)))
  (define path-key (string-append prefix "." (symbol->string key)))
  (define (leaf-or-recur child)
    (if (null? more)
        (if coerce (coerce value ftype) value)
        (if next-type
            (set-typed (ensure-hash child) next-type more value counters coerce path-key)
            (set-steps (ensure-hash child) more value))))
  (define h (ensure-hash js))
  (cond
    [(or (eq? kind 'list) (pair? (fsh-step-brackets s)))
     (define arr (let ([a (hash-ref h key '())]) (if (list? a) a '())))
     (define-values (i url) (array-position arr (fsh-step-brackets s) path-key counters key))
     (define cur (array-ref-or arr i))
     (define seeded (if (and url (not (hash? cur))) (hasheq 'url url) cur))
     (hash-set h key (array-set-pad arr i (leaf-or-recur seeded)))]
    [else (hash-set h key (leaf-or-recur (hash-ref h key #f)))]))

;; array-position : list brackets path-key counters field -> (values index url-or-#f)
(define (array-position arr brackets path-key counters field)
  (define (counter) (hash-ref counters path-key #f))
  (define (set-counter! n) (hash-set! counters path-key n) n)
  (cond
    [(null? brackets) (values 0 #f)]
    [else
     (define b (car brackets))
     (case (fsh-bracket-kind b)
       [(index) (values (set-counter! (fsh-bracket-value b)) #f)]
       [(soft+) (values (set-counter! (let ([c (counter)]) (if c (add1 c) (length arr)))) #f)]
       [(soft=) (values (or (counter) (max 0 (sub1 (length arr)))) #f)]
       [(url slice)
        ;; extension[url] / extension[sliceName]: the element whose url is that value (a slice name of
        ;; a complex extension IS its relative url); a soft index after it (`[url][+]`) picks among
        ;; repetitions of that url
        (define url (fsh-bracket-value b))
        (define rest (cdr brackets))
        (define positions (for/list ([x (in-list arr)] [j (in-naturals)]
                                     #:when (and (hash? x) (equal? (hash-ref x 'url #f) url))) j))
        (define url-key (string-append path-key "[" url "]"))
        (define (url-counter) (hash-ref counters url-key #f))
        (define n
          (cond
            [(and (pair? rest) (eq? (fsh-bracket-kind (car rest)) 'soft+))
             (hash-set! counters url-key (if (url-counter) (add1 (url-counter)) 0)) (hash-ref counters url-key)]
            [(and (pair? rest) (eq? (fsh-bracket-kind (car rest)) 'index))
             (hash-set! counters url-key (fsh-bracket-value (car rest))) (fsh-bracket-value (car rest))]
            [else (or (url-counter) 0)]))
        (values (if (< n (length positions)) (list-ref positions n) (length arr)) url)]
       [else (values 0 #f)])]))

(define (set-steps js steps value)
  (cond
    [(null? steps) value]
    [else
     (define s (car steps)) (define more (cdr steps))
     (define key (step-key s)) (define idx (step-index s))
     (define h (ensure-hash js))
     (cond
       [idx
        (define arr (hash-ref h key '()))
        (hash-set h key (array-set-pad arr idx (set-steps (array-ref-or arr idx) more value)))]
       [else
        (hash-set h key (set-steps (hash-ref h key #f) more value))])]))

;; read the value at the caret path, or #f
(define (caret-ref js caret-str)
  (let loop ([js js] [steps (caret-steps caret-str)])
    (cond
      [(null? steps) js]
      [(not (hash? js)) #f]
      [else
       (define s (car steps)) (define key (step-key s)) (define idx (step-index s))
       (define v (hash-ref js key #f))
       (loop (if idx (array-ref-or v idx) v) (cdr steps))])))

;; ── examples (HtDP living documentation) ─────────────────────────────────────────────────
(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  ;; array helpers tolerate absent / short / non-list inputs
  (check-equal? (array-ref-or '(a b c) 1) 'b)
  (check-equal? (array-ref-or '(a) 3)     #f)
  (check-equal? (array-ref-or #f 0)       #f)
  (check-equal? (array-set-pad '() 2 'x)  (list #f #f 'x))   ; pads with #f
  (check-equal? (array-set-pad '(a b) 0 'z) (list 'z 'b))
  ;; round-trip a caret path through an object + array
  (check-equal? (caret-ref (caret-set (hasheq) "^a.b[1].c" "v") "^a.b[1].c") "v")
  ;; schema-following mode (needs a registered tower — required at the top of this submodule)
  (define vs (caret-set (hasheq) "^jurisdiction.text" "World" #:type 'ValueSet))
  (check-equal? vs (hasheq 'jurisdiction (list (hasheq 'text "World"))))          ; 0..* -> element 0
  (define c (make-hash))
  (define ed (for/fold ([h (hasheq)]) ([cp (in-list '("^extension[http://x/obl][+].extension[code].valueCode"
                                                      "^extension[http://x/obl][=].extension[actor].valueCanonical"
                                                      "^extension[http://x/obl][+].extension[code].valueCode"))]
                                       [v (in-list '("SHALL" "http://a" "SHOULD"))])
               (caret-set h cp v #:type 'ElementDefinition #:counters c)))
  (check-equal? (length (hash-ref ed 'extension)) 2)                                ; two obligations
  (check-equal? (hash-ref (car (hash-ref ed 'extension)) 'url) "http://x/obl")
  (check-equal? (map (lambda (e) (hash-ref e 'url)) (hash-ref (car (hash-ref ed 'extension)) 'extension))
                '("code" "actor")))
