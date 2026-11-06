#lang racket/base

;;; yaml.rkt — the YAML that sushi-config.yaml is written in, read as ordered data.
;;;
;;; Not a full YAML: the subset FSH projects use — block mappings and sequences by indentation,
;;; flow sequences `[a, b]` and mappings `{a: b}`, plain / 'single' / "double" quoted scalars,
;;; block scalars `|` and `>`, and comments. Anchors, aliases, tags and multi-document streams are
;;; not supported and are errors, never silently misread.
;;;
;;; A YamlValue is one of:
;;; - String               a scalar (plain or quoted; numbers and booleans stay strings —
;;;                        `yaml-boolean` and friends convert where a key wants them)
;;; - 'null                an empty value (`key:` with nothing under it, `~`, `null`)
;;; - (listof YamlValue)   a sequence
;;; - (ymap entries)       a mapping; entries (listof (cons String YamlValue)) in document order
;;;
;;; read-yaml raises exn:fail:yaml (message with the 1-based line) on what it cannot read.

(require racket/contract
         racket/list
         racket/match
         racket/string
         racket/file)

(provide
 (struct-out ymap)
 (struct-out exn:fail:yaml)
 (contract-out
  [read-yaml (-> string? any/c)]
  [read-yaml-file (-> path-string? any/c)]
  [yref (->* (any/c string?) (any/c) any/c)]          ; a mapping's value by key, else default
  [yaml-boolean (-> any/c (or/c boolean? #f 'null))]))

(struct ymap (entries) #:transparent)
(struct exn:fail:yaml exn:fail (line) #:transparent)

(define (yaml-error line fmt . args)
  (raise (exn:fail:yaml (format "sushi-config.yaml:~a: ~a" line (apply format fmt args)) (current-continuation-marks) line)))

;; yref : YamlValue String [Any] -> YamlValue
(define (yref m key [default #f])
  (match m
    [(ymap es) (let ([e (assoc key es)]) (if e (cdr e) default))]
    [_ default]))

;; yaml-boolean : YamlValue -> Boolean|'null
(define (yaml-boolean v)
  (match v
    [(or "true" "True" "TRUE" "yes" "on") #t]
    [(or "false" "False" "FALSE" "no" "off") #f]
    [_ 'null]))

(define (read-yaml-file f) (read-yaml (file->string f)))

;; ---- lines -------------------------------------------------------------------------------------

;; A Line is (line no indent text): text without its indentation, comment or trailing space
(struct line (no indent text raw) #:transparent)

;; strip-comment : String -> String   a `#` starts a comment at the line start or after a space,
;; outside quotes
(define (strip-comment s)
  (let loop ([i 0] [q #f])
    (cond
      [(= i (string-length s)) s]
      [else
       (define c (string-ref s i))
       (cond
         [q (loop (add1 i) (if (char=? c q) #f q))]
         [(memv c '(#\" #\')) (if (or (= i 0) (memv (string-ref s (sub1 i)) '(#\space #\: #\[ #\{ #\, #\-))) (loop (add1 i) c) (loop (add1 i) #f))]
         [(and (char=? c #\#) (or (= i 0) (char-whitespace? (string-ref s (sub1 i))))) (substring s 0 i)]
         [else (loop (add1 i) #f)])])))

(define (lines-of text)
  (for/list ([raw (string-split (string-replace text "\r\n" "\n") "\n" #:trim? #f)] [no (in-naturals 1)])
    (when (regexp-match? #rx"^\t" raw) (yaml-error no "tabs are not allowed for indentation"))
    (define body (string-trim (strip-comment raw) #:left? #f))
    (define indent (string-length (car (regexp-match #px"^ *" raw))))
    (line no indent (string-trim body #:right? #f) raw)))

(define (blank? l) (string=? (line-text l) ""))

;; ---- the parser --------------------------------------------------------------------------------

;; read-yaml : String -> YamlValue
(define (read-yaml text)
  (define all (lines-of text))
  (for ([l all])
    (when (regexp-match? #px"^(---|\\.\\.\\.)(\\s|$)" (line-text l))
      (unless (= (line-no l) 1) (yaml-error (line-no l) "multi-document YAML is not supported"))))
  (define ls (filter (lambda (l) (not (and (= (line-no l) 1) (regexp-match? #px"^---\\s*$" (line-text l))))) all))
  (define-values (v rest) (parse-block ls -1))
  (define left (dropf rest blank?))
  (unless (null? left) (yaml-error (line-no (car left)) "unexpected content: ~a" (line-text (car left))))
  v)

;; parse-block : (listof Line) Integer -> (values YamlValue (listof Line))   the block at an indent
;; greater than `parent`
(define (parse-block ls parent)
  (define ls* (dropf ls blank?))
  (cond
    [(null? ls*) (values 'null ls*)]
    [(<= (line-indent (car ls*)) parent) (values 'null ls*)]
    [else
     (define l (car ls*))
     (if (sequence-item? (line-text l))
         (parse-sequence ls* (line-indent l))
         (parse-mapping ls* (line-indent l)))]))

(define (sequence-item? t) (or (string=? t "-") (string-prefix? t "- ")))

;; parse-sequence : Lines Indent -> (values (listof YamlValue) Lines)
(define (parse-sequence ls ind)
  (let loop ([ls ls] [acc '()])
    (define ls* (dropf ls blank?))
    (cond
      [(or (null? ls*) (not (= (line-indent (car ls*)) ind)) (not (sequence-item? (line-text (car ls*)))))
       (when (and (pair? ls*) (> (line-indent (car ls*)) ind))
         (yaml-error (line-no (car ls*)) "unexpected indentation"))
       (values (reverse acc) ls*)]
      [else
       (define l (car ls*))
       (define after (string-trim (substring (line-text l) 1) #:right? #f))
       (cond
         [(string=? after "")
          (define-values (v rest) (parse-block (cdr ls*) ind))
          (loop rest (cons v acc))]
         [else
          ;; `- key: v` opens a mapping whose further keys sit at the item's content column
          (define col (+ ind (- (string-length (line-text l)) (string-length after))))
          (define synthetic (line (line-no l) col after (line-raw l)))
          (cond
            [(mapping-entry? after)
             (define-values (v rest) (parse-mapping (cons synthetic (cdr ls*)) col))
             (loop rest (cons v acc))]
            [else
             (define-values (v rest) (scalar-or-block synthetic (cdr ls*) ind))
             (loop rest (cons v acc))])])])))

;; mapping-entry? : String -> Boolean   `key: …` or `key:` (a key may be quoted)
(define (mapping-entry? t)
  (and (not (regexp-match? #px"^[\\[{]" t))
       (or (regexp-match? #px"^\"(?:[^\"\\\\]|\\\\.)*\"\\s*:(\\s|$)" t)
           (regexp-match? #px"^'(?:[^']|'')*'\\s*:(\\s|$)" t)
           (regexp-match? #px"^[^\"'#][^#]*?:(\\s|$)" t))))

;; split-entry : String Integer -> (values key rest)
(define (split-entry t no)
  (match t
    [(pregexp #px"^\"((?:[^\"\\\\]|\\\\.)*)\"\\s*:\\s*(.*)$" (list _ k r)) (values (unescape k no) r)]
    [(pregexp #px"^'((?:[^']|'')*)'\\s*:\\s*(.*)$" (list _ k r)) (values (string-replace k "''" "'") r)]
    [(pregexp #px"^(.*?):(?:\\s+(.*))?$" (list _ k r)) (values (string-trim k) (or r ""))]
    [_ (yaml-error no "expected `key: value`: ~a" t)]))

;; parse-mapping : Lines Indent -> (values ymap Lines)
(define (parse-mapping ls ind)
  (let loop ([ls ls] [acc '()])
    (define ls* (dropf ls blank?))
    (cond
      [(or (null? ls*) (< (line-indent (car ls*)) ind)) (values (ymap (reverse acc)) ls*)]
      [(> (line-indent (car ls*)) ind) (yaml-error (line-no (car ls*)) "unexpected indentation")]
      [(sequence-item? (line-text (car ls*))) (values (ymap (reverse acc)) ls*)]
      [else
       (define l (car ls*))
       (unless (mapping-entry? (line-text l)) (yaml-error (line-no l) "expected `key: value`: ~a" (line-text l)))
       (define-values (k r) (split-entry (line-text l) (line-no l)))
       (when (assoc k acc) (yaml-error (line-no l) "duplicate key ~a" k))
       (define-values (v rest)
         (cond
           [(string=? r "")
            ;; a nested block; a sequence may sit at the key's own indentation (common in YAML)
            (define next (dropf (cdr ls*) blank?))
            (if (and (pair? next) (= (line-indent (car next)) ind) (sequence-item? (line-text (car next))))
                (parse-sequence next ind)
                (parse-block (cdr ls*) ind))]
           [else (scalar-or-block (line (line-no l) ind r (line-raw l)) (cdr ls*) ind)]))
       (loop rest (cons (cons k v) acc))])))

;; scalar-or-block : Line Lines Indent -> (values YamlValue Lines)   the value text of a line:
;; a flow collection, a block scalar indicator, or a scalar
(define (scalar-or-block l rest parent)
  (define t (line-text l))
  (cond
    [(regexp-match? #px"^[|>][+-]?$" t) (block-scalar (string-ref t 0) (substring t 1) rest parent)]
    [(regexp-match? #px"^[&*!]" t) (yaml-error (line-no l) "anchors, aliases and tags are not supported")]
    [(regexp-match? #px"^[\\[{]" t)
     (define-values (v pos) (flow t 0 (line-no l)))
     (unless (string=? (string-trim (substring t pos)) "") (yaml-error (line-no l) "unexpected text after a flow collection"))
     (values v rest)]
    [else (values (scalar t (line-no l)) rest)]))

;; block-scalar : Char String Lines Indent -> (values String Lines)   | keeps newlines, > folds them
(define (block-scalar style chomp ls parent)
  (define-values (taken rest)
    (let loop ([ls ls] [acc '()])
      (cond [(null? ls) (values (reverse acc) ls)]
            [(blank? (car ls)) (loop (cdr ls) (cons (car ls) acc))]
            [(> (line-indent (car ls)) parent) (loop (cdr ls) (cons (car ls) acc))]
            [else (values (reverse acc) ls)])))
  (define content (reverse (dropf (reverse taken) blank?)))
  (define ind (if (null? content) 0 (apply min (map line-indent (filter (lambda (l) (not (blank? l))) content)))))
  (define texts (for/list ([l content]) (if (blank? l) "" (substring (line-raw l) (min ind (string-length (line-raw l)))))))
  (define joined
    (if (char=? style #\|)
        (string-join texts "\n")
        (string-join (for/list ([para (string-split (string-join texts "\n") "\n\n")]) (string-join (string-split para "\n") " ")) "\n")))
  (values (if (equal? chomp "-") joined (string-append joined "\n")) rest))

;; scalar : String Integer -> YamlValue
(define (scalar t no)
  (match t
    [(or "~" "null" "Null" "NULL") 'null]
    [(pregexp #px"^\"((?:[^\"\\\\]|\\\\.)*)\"$" (list _ s)) (unescape s no)]
    [(pregexp #px"^'((?:[^']|'')*)'$" (list _ s)) (string-replace s "''" "'")]
    [(pregexp #px"^[\"']") (yaml-error no "unterminated quoted string")]
    [_ t]))

(define (unescape s no)
  (regexp-replace* #px"\\\\(.)" s
                   (lambda (_ c) (match c ["n" "\n"] ["t" "\t"] ["\"" "\""] ["\\" "\\"] ["/" "/"] ["r" "\r"]
                                   [_ (yaml-error no "unsupported escape \\~a" c)]))))

;; flow : String Index Line -> (values YamlValue next-index)   [a, b] and {k: v} on one line
(define (flow t i no)
  (define (skip i) (if (and (< i (string-length t)) (char-whitespace? (string-ref t i))) (skip (add1 i)) i))
  (define (item i stops)  ; -> (values value index)
    (define j (skip i))
    (cond
      [(>= j (string-length t)) (yaml-error no "unterminated flow collection")]
      [(memv (string-ref t j) '(#\[ #\{)) (flow t j no)]
      [(memv (string-ref t j) '(#\" #\'))
       (define q (string-ref t j))
       (define k (let loop ([k (add1 j)])
                   (cond [(>= k (string-length t)) (yaml-error no "unterminated quoted string")]
                         [(and (char=? q #\") (char=? (string-ref t k) #\\)) (loop (+ k 2))]
                         [(char=? (string-ref t k) q)
                          (if (and (char=? q #\') (< (add1 k) (string-length t)) (char=? (string-ref t (add1 k)) #\')) (loop (+ k 2)) k)]
                         [else (loop (add1 k))])))
       (values (scalar (substring t j (add1 k)) no) (add1 k))]
      [else
       (define k (let loop ([k j]) (if (or (>= k (string-length t)) (memv (string-ref t k) stops)) k (loop (add1 k)))))
       (values (scalar (string-trim (substring t j k)) no) k)]))
  (define open (string-ref t i))
  (define close (if (char=? open #\[) #\] #\}))
  (let loop ([i (add1 i)] [acc '()])
    (define j (skip i))
    (cond
      [(>= j (string-length t)) (yaml-error no "unterminated flow collection")]
      [(char=? (string-ref t j) close)
       (values (if (char=? open #\[) (reverse acc) (ymap (reverse acc))) (add1 j))]
      [(char=? open #\[)
       (define-values (v k) (item j '(#\, #\])))
       (define k* (skip k))
       (loop (if (and (< k* (string-length t)) (char=? (string-ref t k*) #\,)) (add1 k*) k*) (cons v acc))]
      [else
       (define-values (key k) (item j '(#\:)))
       (define k* (skip k))
       (unless (and (< k* (string-length t)) (char=? (string-ref t k*) #\:)) (yaml-error no "expected `:` in a flow mapping"))
       (define-values (v m) (item (add1 k*) '(#\, #\})))
       (define m* (skip m))
       (loop (if (and (< m* (string-length t)) (char=? (string-ref t m*) #\,)) (add1 m*) m*)
             (cons (cons (if (string? key) key (format "~a" key)) v) acc))])))

(module+ test
  (require rackunit)
  (define (y s) (read-yaml s))
  ;; scalars, quoting, comments
  (check-equal? (y "a: 1\nb: \"x # not a comment\"\nc: 'it''s'  # a comment\nd:\ne: ~\n")
                (ymap '(("a" . "1") ("b" . "x # not a comment") ("c" . "it's") ("d" . null) ("e" . null))))
  (check-equal? (y "url: http://example.org/a#b\n") (ymap '(("url" . "http://example.org/a#b"))))
  ;; nested mappings keep their order; keys with dots and slashes (pages)
  (check-equal? (y "pages:\n  index.md:\n    title: Home\n  operation.md:\n    title: The operation\n")
                (ymap (list (cons "pages" (ymap (list (cons "index.md" (ymap '(("title" . "Home"))))
                                                      (cons "operation.md" (ymap '(("title" . "The operation"))))))))))
  ;; sequences: of scalars, of mappings, at the key's own indentation
  (check-equal? (y "fhirVersion:\n  - 4.0.1\n") (ymap '(("fhirVersion" . ("4.0.1")))))
  (check-equal? (y "fhirVersion:\n- 4.0.1\n") (ymap '(("fhirVersion" . ("4.0.1")))))
  (check-equal? (y "contact:\n  - name: A\n    telecom:\n      - system: url\n        value: http://a\n  - name: B\n")
                (ymap (list (cons "contact" (list (ymap (list (cons "name" "A")
                                                              (cons "telecom" (list (ymap '(("system" . "url") ("value" . "http://a")))))))
                                                  (ymap '(("name" . "B"))))))))
  ;; flow collections
  (check-equal? (y "fhirVersion: [4.0.1, \"5.0.0\"]\n") (ymap '(("fhirVersion" . ("4.0.1" "5.0.0")))))
  (check-equal? (y "dep: { version: 6.1.0, id: x }\n") (ymap (list (cons "dep" (ymap '(("version" . "6.1.0") ("id" . "x")))))))
  (check-equal? (y "empty: []\n") (ymap '(("empty" . ()))))
  ;; block scalars
  (check-equal? (y "description: |\n  line one\n  line two\nnext: x\n") (ymap '(("description" . "line one\nline two\n") ("next" . "x"))))
  (check-equal? (y "description: >-\n  folded\n  text\n\n  para\n") (ymap '(("description" . "folded text\npara"))))
  ;; a leading document marker is allowed
  (check-equal? (y "---\na: b\n") (ymap '(("a" . "b"))))
  ;; refusals
  (check-exn #rx":2: duplicate key a" (lambda () (y "a: 1\na: 2\n")))
  (check-exn #rx"anchors" (lambda () (y "a: &x 1\n")))
  (check-exn #rx"tabs" (lambda () (y "a:\n\tb: 1\n")))
  (check-exn #rx"unexpected indentation" (lambda () (y "a: 1\n   b: 2\n")))
  ;; helpers
  (check-equal? (yref (y "a: b\n") "a") "b")
  (check-false (yref (y "a: b\n") "z"))
  (check-equal? (yaml-boolean "true") #t))
