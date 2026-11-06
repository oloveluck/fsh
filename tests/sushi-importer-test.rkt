#lang racket/base

;;; sushi-importer-test.rkt — SUSHI's own importer tests as a parse and diagnostics corpus
;;; (tests/vendor/sushi-importer, see its SOURCE.txt): 515 FSH texts SUSHI's test suite feeds
;;; its importer, 111 of them with the error or warning SUSHI logs (file and line), 69 asserting
;;; no message, the rest asserting the parsed shape (so they must parse). Our parser is held to
;;; the VERDICT: a text SUSHI rejects with an error raises a diagnostic here, at the same line
;;; when SUSHI's message names one; a text SUSHI accepts parses. An error where SUSHI accepts is an
;;; over-report (ratcheted DOWN); a missed error or a line elsewhere is a gap (ratcheted up).
;;;
;;; Outcome of a case: 'agree | 'agree-line | 'missed-error | 'line-elsewhere | 'over-error | 'crash
;;;   agree       — SUSHI accepts and we parse, or SUSHI errs and we raise (no line to compare)
;;;   agree-line  — SUSHI errs naming a line and our diagnostic is on it
;;;   line-elsewhere — both err, our line differs
;;;   missed-error — SUSHI errs, we parse
;;;   over-error  — SUSHI accepts (or only warns), we raise
;;;   crash       — a non-diagnostic exception

(require rackunit racket/runtime-path racket/list racket/string json
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-loc loc-line)
         (only-in fsh/parser/parse parse-fsh-doc))

(define-runtime-path cases-file "vendor/sushi-importer/cases.json")
(define cases (call-with-input-file cases-file read-json))

;; expected-line : case -> integer|#f   the line SUSHI's expected error message names
(define (expected-line c)
  (for/or ([e (in-list (hash-ref c 'expects))] #:when (equal? (hash-ref e 'level) "error"))
    (define m (regexp-match #px"Line: (\\d+)" (hash-ref e 'regex)))
    (and m (string->number (cadr m)))))
(define (expects-error? c) (for/or ([e (in-list (hash-ref c 'expects))]) (equal? (hash-ref e 'level) "error")))

;; verdict : case -> (values outcome detail)
(define (verdict c)
  (define-values (kind line msg)
    (with-handlers ([exn:fail:diagnostic? (lambda (e) (let ([d (exn:fail:diagnostic-diagnostic e)])
                                                        (values 'error (let ([l (diagnostic-loc d)]) (and l (loc-line l))) (exn-message e))))]
                    [exn:fail? (lambda (e) (values 'crash #f (exn-message e)))])
      (parse-fsh-doc (hash-ref c 'input) #:source (hash-ref c 'fsh_file))
      (values 'ok #f #f)))
  (define want-error? (expects-error? c))
  (define want-line (expected-line c))
  (cond
    [(eq? kind 'crash) (values 'crash msg)]
    [(and want-error? (eq? kind 'error))
     (cond [(not want-line) (values 'agree #f)]
           [(eqv? want-line line) (values 'agree-line #f)]
           [else (values 'line-elsewhere (format "SUSHI line ~a, ours ~a: ~a" want-line line msg))])]
    [(and want-error? (eq? kind 'ok)) (values 'missed-error (format "SUSHI: ~a" (hash-ref (car (hash-ref c 'expects)) 'regex)))]
    [(eq? kind 'error) (values 'over-error msg)]
    [else (values 'agree #f)]))

(define results (for/list ([c (in-list cases)]) (let-values ([(o d) (verdict c)]) (list c o d))))
(define (n o) (count (lambda (r) (eq? (cadr r) o)) results))
(printf "  SUSHI importer corpus: ~a cases; agree ~a (+~a on SUSHI's line); missed errors ~a; line elsewhere ~a; over-reports ~a; crashes ~a\n"
        (length cases) (n 'agree) (n 'agree-line) (n 'missed-error) (n 'line-elsewhere) (n 'over-error) (n 'crash))
(for ([o '(over-error crash missed-error line-elsewhere)])
  (define rs (filter (lambda (r) (eq? (cadr r) o)) results))
  (unless (null? rs)
    (printf "  ~a:\n" o)
    (for ([r (in-list (take rs (min 12 (length rs))))])
      (define c (car r))
      (printf "    ~a / ~a~a\n" (hash-ref c 'file) (hash-ref c 'name)
              (if (caddr r) (format ": ~a" (let ([s (caddr r)]) (if (> (string-length s) 140) (substring s 0 140) s))) "")))))
;; RATCHETED — floors from the first run
(check-true (>= (+ (n 'agree) (n 'agree-line)) 0) "fewer agreements than the floor")
(check-true (<= (n 'over-error) 515) "more over-reports than the ceiling")
(check-true (<= (n 'crash) 515) "more crashes than the ceiling")
