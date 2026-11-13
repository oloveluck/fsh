#lang racket/base

;;; sushi-importer-test.rkt — SUSHI's own importer tests as a parse and diagnostics corpus
;;; (tests/vendor/sushi-importer, see its SOURCE.txt): 515 FSH texts SUSHI's test suite feeds
;;; its importer, 111 of them with the error or warning SUSHI logs (file and line), 69 asserting
;;; no message, the rest asserting the parsed shape (so they must parse). Our parser is held to
;;; the VERDICT: a text SUSHI rejects with an error raises a diagnostic here, at the same line
;;; when SUSHI's message names one; a text SUSHI accepts parses. An error where SUSHI accepts is an
;;; over-report (ratcheted DOWN); a missed error or a line elsewhere is a gap (ratcheted up).
;;; Our side is `parse-fsh-doc/diagnostics`: every entity's parse error (indentation included) and the
;;; import-level checks of fsh/lint (duplicates, spacing, missing parts, filter value kinds).
;;; A case asserting only the parsed shape (no_messages false, no expectation) may still have made SUSHI
;;; log an error its test does not look at — `* address 1..* Address` with no short does (SUSHI's grammar
;;; requires it; BadDefs.fsh asserts the error) — so an over-report there is read against SUSHI's source.
;;;
;;; Outcome of a case: 'agree | 'agree-line | 'missed-error | 'line-elsewhere | 'over-error | 'crash
;;;   agree       — SUSHI accepts and we parse, or SUSHI errs and we raise (no line to compare)
;;;   agree-unasserted — we err on a text SUSHI's importer also rejects, though that test does not say so
;;;   agree-line  — SUSHI errs naming a line and our diagnostic is on it
;;;   line-elsewhere — both err, our line differs
;;;   missed-error — SUSHI errs, we parse
;;;   over-error  — SUSHI accepts (or only warns), we raise
;;;   crash       — a non-diagnostic exception

(require rackunit racket/runtime-path racket/list racket/string json
         (only-in fhir/diagnostic diagnostic-severity diagnostic-loc diagnostic->string loc-line)
         (only-in fsh/parser/parse parse-fsh-doc/diagnostics))

(define-runtime-path cases-file "vendor/sushi-importer/cases.json")
(define cases (call-with-input-file cases-file read-json))

;; left-align : string -> string   SUSHI's tests write many inputs indented as the TypeScript is, and
;; SUSHI "processes the first rule as if it is not indented" (logging an error the test ignores); the
;; common indentation is removed here so the corpus measures what the tests assert
(define (left-align text)
  (define lines (string-split text "\n" #:trim? #f))
  (define indents (for/list ([l (in-list lines)] #:unless (string=? (string-trim l) ""))
                    (string-length (car (regexp-match #px"^[ \t]*" l)))))
  (define n (if (null? indents) 0 (apply min indents)))
  (string-join (for/list ([l (in-list lines)]) (if (>= (string-length l) n) (substring l n) (string-trim l))) "\n"))

;; sushi-logs-anyway? : case -> boolean   tests whose text makes SUSHI's importer log an error the test
;; does not assert (a sibling test asserts it on the same shape): an error here agrees with SUSHI
(define (sushi-logs-anyway? c)
  (define name (hash-ref c 'name))
  (or (regexp-match? #rx"should only apply each metadata attribute the first time it is declared" name) ; the duplicate-metadata error
      (and (equal? (hash-ref c 'file) "FSHImporter.Logical.test.ts")                                      ; the missing-short error
           (equal? name "should parse complete add element rules"))
      (regexp-match? #rx"should log a warning when a data type is defined with a flag value" name)       ; "extraneous input" after the flag
      (regexp-match? #rx"should log a warning for missing targetType with flags and docs" name)
      ;; `Target: http://example.org/target`, unquoted: FSH.g4 is `target: KW_TARGET STRING`, so SUSHI's
      ;; parser logs "mismatched input … expecting STRING" and recovers; the test looks at the rule only
      (and (equal? (hash-ref c 'file) "FSHImporter.Mapping.test.ts") (equal? name "should parse a pathRule"))))

;; expected-line : case -> integer|#f   the line SUSHI's expected error message names
(define (expected-line c)
  (for/or ([e (in-list (hash-ref c 'expects))] #:when (equal? (hash-ref e 'level) "error"))
    (define m (regexp-match #px"Line: (\\d+)" (hash-ref e 'regex)))
    (and m (string->number (cadr m)))))
(define (expects-error? c) (for/or ([e (in-list (hash-ref c 'expects))]) (equal? (hash-ref e 'level) "error")))

;; verdict : case -> (values outcome detail)   our errors: each entity's parse error and fsh/lint's
(define (verdict c)
  (define-values (kind lines msg)
    (with-handlers ([exn:fail? (lambda (e) (values 'crash '() (exn-message e)))])
      (define-values (_entities ds) (parse-fsh-doc/diagnostics (left-align (hash-ref c 'input)) #:source (hash-ref c 'fsh_file)))
      (define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) ds))
      (if (null? errors)
          (values 'ok '() #f)
          (values 'error (for/list ([d (in-list errors)]) (let ([l (diagnostic-loc d)]) (and l (loc-line l))))
                  (string-join (map diagnostic->string errors) " | ")))))
  (define want-error? (expects-error? c))
  (define want-line (expected-line c))
  (cond
    [(eq? kind 'crash) (values 'crash msg)]
    [(and want-error? (eq? kind 'error))
     (cond [(not want-line) (values 'agree #f)]
           [(memv want-line lines) (values 'agree-line #f)]
           [else (values 'line-elsewhere (format "SUSHI line ~a, ours ~a: ~a" want-line lines msg))])]
    [(and want-error? (eq? kind 'ok)) (values 'missed-error (format "SUSHI: ~a" (hash-ref (car (hash-ref c 'expects)) 'regex)))]
    [(and (eq? kind 'error) (sushi-logs-anyway? c)) (values 'agree-unasserted msg)]
    [(eq? kind 'error) (values 'over-error msg)]
    [else (values 'agree #f)]))

(define results (for/list ([c (in-list cases)]) (let-values ([(o d) (verdict c)]) (list c o d))))
(define (n o) (count (lambda (r) (eq? (cadr r) o)) results))
(printf "  SUSHI importer corpus: ~a cases; agree ~a (+~a on SUSHI's line, +~a where SUSHI logs an error its test leaves unasserted); missed errors ~a; line elsewhere ~a; over-reports ~a; crashes ~a\n"
        (length cases) (n 'agree) (n 'agree-line) (n 'agree-unasserted) (n 'missed-error) (n 'line-elsewhere) (n 'over-error) (n 'crash))
(for ([o '(over-error crash missed-error line-elsewhere)])
  (define rs (filter (lambda (r) (eq? (cadr r) o)) results))
  (unless (null? rs)
    (printf "  ~a:\n" o)
    (for ([r (in-list (take rs (min 80 (length rs))))])
      (define c (car r))
      (printf "    ~a / ~a~a\n" (hash-ref c 'file) (hash-ref c 'name)
              (if (caddr r) (format ": ~a" (let ([s (caddr r)]) (if (> (string-length s) 200) (substring s 0 200) s))) "")))))
;; RATCHETED — agreements up, misses and over-reports down (407 + 95 + 12 of 515; the one
;; line elsewhere is the empty RuleSet, whose error SUSHI positions at the NEXT entity)
(check-true (>= (+ (n 'agree) (n 'agree-line) (n 'agree-unasserted)) 514) "fewer agreements than the floor")
(check-true (<= (n 'missed-error) 0) "more missed errors than the ceiling")
(check-true (<= (n 'line-elsewhere) 1) "more lines elsewhere than the ceiling")
(check-true (<= (n 'over-error) 0) "more over-reports than the ceiling")
(check-true (<= (n 'crash) 0) "more crashes than the ceiling")
