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
         (only-in racket/list make-list list-set)
         racket/contract
         "parser/path.rkt")

(provide
 (contract-out
  [caret-set   (-> any/c string? any/c any/c)]   ; jsexpr, "^a.b[0].c", value -> updated jsexpr
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
(define (caret-set js caret-str value)
  (set-steps (ensure-hash js) (caret-steps caret-str) value))

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
  (require rackunit)
  ;; array helpers tolerate absent / short / non-list inputs
  (check-equal? (array-ref-or '(a b c) 1) 'b)
  (check-equal? (array-ref-or '(a) 3)     #f)
  (check-equal? (array-ref-or #f 0)       #f)
  (check-equal? (array-set-pad '() 2 'x)  (list #f #f 'x))   ; pads with #f
  (check-equal? (array-set-pad '(a b) 0 'z) (list 'z 'b))
  ;; round-trip a caret path through an object + array
  (check-equal? (caret-ref (caret-set (hasheq) "^a.b[1].c" "v") "^a.b[1].c") "v"))
