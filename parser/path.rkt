#lang racket/base

;;; path.rkt — the FSH path sub-parser.
;;;
;;; FSH `path`/`caretPath` tokens are single SEQUENCE/CARET_SEQUENCE strings; their internal
;;; structure is parsed here. This is where FSH "extends" FHIRPath navigation: dotted steps
;;; and numeric indices are the FHIRPath-shared part; `[sliceName]`, soft-index `[+]`/`[=]`,
;;; type/reference `[Type]`, and caret `^path` are the FSH additions.

(require racket/contract)

(provide (struct-out fsh-path)
         (struct-out fsh-step)
         (struct-out fsh-bracket)
         (struct-out fsh-caret)
         (contract-out
          [parse-fsh-path  (-> string? fsh-path?)]    ; "a.b[0]" -> a path
          [parse-fsh-caret (-> string? fsh-caret?)])) ; "^a.b" -> a caret path

(struct fsh-path    (steps)         #:prefab)  ; steps : (listof fsh-step)
(struct fsh-step    (name brackets) #:prefab)  ; brackets : (listof fsh-bracket)
(struct fsh-bracket (kind value)    #:prefab)  ; kind ∈ 'slice 'index 'soft+ 'soft= 'url
(struct fsh-caret   (steps)         #:prefab)

(define (parse-fsh-path str) (fsh-path (scan-steps str)))

;; a caret path string includes the leading "^"
(define (parse-fsh-caret str)
  (fsh-caret (scan-steps (if (and (> (string-length str) 0) (char=? (string-ref str 0) #\^))
                             (substring str 1) str))))

(define (scan-steps str)
  (define len (string-length str))
  (let loop ([i 0] [steps '()])
    (cond
      [(>= i len) (reverse steps)]
      [else
       (define-values (step next) (scan-step str i len))
       (loop (if (and (< next len) (char=? (string-ref str next) #\.)) (add1 next) next)
             (cons step steps))])))

(define (scan-step str i len)
  (define name-end
    (let loop ([j i])
      (cond [(>= j len) j]
            [(memv (string-ref str j) '(#\. #\[)) j]
            [else (loop (add1 j))])))
  (define name (substring str i name-end))
  (let loop ([j name-end] [brs '()])
    (cond
      [(and (< j len) (char=? (string-ref str j) #\[))
       (define close (find-close str (add1 j) len))
       (loop (min len (add1 close))
             (cons (classify-bracket (substring str (add1 j) (min close len))) brs))]
      [else (values (fsh-step name (reverse brs)) j)])))

(define (find-close str j len)
  (let loop ([j j])
    (cond [(>= j len) len]
          [(char=? (string-ref str j) #\]) j]
          [else (loop (add1 j))])))

;; bracket content is classified syntactically; slice vs type is resolved later, by context
(define (classify-bracket c)
  (cond
    [(string=? c "x") (fsh-bracket 'choice #f)]      ; value[x] — the choice-type marker
    [(string=? c "+") (fsh-bracket 'soft+ #f)]
    [(string=? c "=") (fsh-bracket 'soft= #f)]
    [(regexp-match? #px"^[0-9]+$" c) (fsh-bracket 'index (string->number c))]
    [(regexp-match? #px"://|^http|^urn:" c) (fsh-bracket 'url c)]
    [else (fsh-bracket 'slice c)]))
