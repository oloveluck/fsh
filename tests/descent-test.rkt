#lang racket/base

;;; descent-test.rkt — the recursive-descent reader (parser/descent.rkt) held to the grammar.
;;;
;;; parse.rkt reads an entity with descent.rkt and hands to brag (parser/grammar.rkt, from SUSHI's
;;; FSH.g4) only what descent does not take: an entity whose header it does not take, and each rule
;;; it does not take. This test keeps brag as the reference, three ways:
;;;   - a document: `parse-fsh-doc/diagnostics` and brag's alone (`…/brag`) give the same entities
;;;     and the same diagnostics, word for word and at the same place;
;;;   - an entity (its tokens, as parse.rkt splits a document): the production reading and brag's
;;;     rule-by-rule reading give the same datum, or both fail;
;;;   - one token list as brag's parser is given it (an entity's header, and the header with each
;;;     rule): wherever descent reads it whole, the grammar's own parse is the same datum.
;;; Texts: every .fsh file the repository vendors (IPS, mCODE, Genomics Reporting, Plan-Net, HRex,
;;; the SUSHI golden corpus), SUSHI's 515 importer texts (as written and left-aligned), every FSH
;;; block of guide.md and ig-guide.md; every value form in every place a value stands; entities
;;; grown from the grammar's productions (seeded); and token soups, mostly not FSH, where a reader
;;; that accepts too much would show.
;;; It also counts how much descent read itself (the rest went to brag): a reader that refused
;;; everything would pass the agreement and buy nothing, so its share has a floor.

(require rackunit racket/runtime-path racket/file racket/list racket/string json
         (only-in brag/support token token-struct? token-struct-type token-struct-skip?)
         (only-in fhir/diagnostic diagnostic->string exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in "../parser/lexer.rkt" tokenize)
         (only-in "../parser/ast.rkt" code-tok)
         (only-in "../parser/descent.rkt" descend-entity descend-parts)
         (only-in "../parser/parse.rkt" parse-fsh-doc/diagnostics parse-fsh-doc/diagnostics/brag)
         (submod "../parser/parse.rkt" grammar)
         (prefix-in g: "../parser/grammar.rkt"))

(define-runtime-path here ".")
(define-runtime-path vendor "vendor")

;; ---- tokens and entities -------------------------------------------------------------------------
;; reals : String -> (listof Token) | #f   the text's tokens without spaces and comments (#f: it
;; does not lex)
(define (reals s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define next (tokenize (open-input-string s)))
    (let loop ()
      (define t (next))
      (cond [(or (void? t) (eof-object? t)) '()] [(token-struct-skip? t) (loop)] [else (cons t (loop))]))))
(define entity-start-types
  '(KW_ALIAS KW_PROFILE KW_EXTENSION KW_INSTANCE KW_INVARIANT KW_VALUESET KW_CODESYSTEM KW_RULESET KW_MAPPING KW_LOGICAL KW_RESOURCE))
(define (type-of t) (token-struct-type t))
;; entities : (listof Token) -> (listof (listof Token))   split as parse.rkt splits a document,
;; without the parameterized RuleSet definitions (captured raw, not parsed)
(define (entities ts)
  (define chunks
    (let loop ([ts ts] [cur '()] [acc '()])
      (cond [(null? ts) (reverse (if (null? cur) acc (cons (reverse cur) acc)))]
            [(and (memq (type-of (car ts)) entity-start-types) (pair? cur)) (loop (cdr ts) (list (car ts)) (cons (reverse cur) acc))]
            [else (loop (cdr ts) (cons (car ts) cur) acc)])))
  (filter (lambda (ch) (not (and (>= (length ch) 2) (eq? (type-of (car ch)) 'KW_RULESET)
                                 (eq? (type-of (cadr ch)) 'PARAM_RULESET_REFERENCE))))
          chunks))
;; header+rules : (listof Token) -> (listof (listof Token))   the header, then the header with each rule
(define (header+rules ch)
  (define groups (let loop ([ts ch] [cur '()] [acc '()])
                   (cond [(null? ts) (reverse (cons (reverse cur) acc))]
                         [(eq? (type-of (car ts)) 'STAR) (loop (cdr ts) (list (car ts)) (cons (reverse cur) acc))]
                         [else (loop (cdr ts) (cons (car ts) cur) acc)])))
  (cons (car groups) (for/list ([g (in-list (cdr groups))]) (append (car groups) g))))

;; ---- the outcomes -------------------------------------------------------------------------------
;; An Outcome is (list 'ok Any) | (list 'error String)
(define (outcome thunk)
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (list 'error (diagnostic->string (exn:fail:diagnostic-diagnostic e))))]
                  [exn:fail? (lambda (e) (list 'error (exn-message e)))])
    (list 'ok (thunk))))
;; document-outcome : (String -> (values entities diagnostics)) String -> Outcome
(define (document-outcome read s)
  (outcome (lambda () (let-values ([(es ds) (read s #:source "t.fsh")]) (list es (map diagnostic->string ds))))))

;; A Tally is (tally docs entities entities-brag-ok entities-whole rules rules-descended token-lists
;; token-lists-descended disagreements)
(struct tally (docs entities brag-ok whole rules descended lists lists-descended disagreements) #:transparent)
(define empty-tally (tally 0 0 0 0 0 0 0 0 '()))
;; grammar-datum : (listof Token) -> Outcome   brag's own parse of one token list, remembered by the
;; list's types (a parse is decided by them; the values ride along — the test compares one list of
;; each shape, which keeps it to seconds)
(define seen-shapes (make-hash))

;; compare : (listof String) -> Tally   every text both ways, at the three levels
(define (compare texts)
  (for/fold ([t empty-tally]) ([s (in-list (remove-duplicates texts))])
    (define production (document-outcome parse-fsh-doc/diagnostics s))
    (define reference (document-outcome parse-fsh-doc/diagnostics/brag s))
    (define t1 (struct-copy tally t [docs (add1 (tally-docs t))]
                            [disagreements (if (equal? production reference) (tally-disagreements t)
                                               (cons (list 'document s production reference) (tally-disagreements t)))]))
    (define ts (reals s))
    (for/fold ([t t1]) ([ch (in-list (if ts (entities ts) '()))])
      (compare-entity ch t s))))

;; compare-entity : (listof Token) Tally Any -> Tally
(define (compare-entity ch t label)
  (define production (outcome (lambda () (parse-reals ch))))
  (define reference (outcome (lambda () (parse-reals/brag ch))))
  (define parts (descend-parts ch))
  (define rules (if parts (cdr parts) '()))
  (define-values (lists lists-descended list-disagreements)
    (for/fold ([n 0] [d 0] [bad '()]) ([ts (in-list (header+rules ch))])
      (define shape (map type-of ts))
      (cond
        [(hash-ref seen-shapes shape #f) (values n d bad)]
        [else
         (hash-set! seen-shapes shape #t)
         (define fast (descend-entity ts))
         (define slow (outcome (lambda () (g:parse-to-datum ts))))
         (values (add1 n) (if fast (add1 d) d)
                 (if (and fast (not (equal? (list 'ok fast) slow))) (cons (list 'token-list label shape fast slow) bad) bad))])))
  (struct-copy tally t
               [entities (add1 (tally-entities t))]
               [brag-ok (+ (tally-brag-ok t) (if (eq? (car reference) 'ok) 1 0))]
               [whole (+ (tally-whole t) (if (descend-entity ch) 1 0))]
               [rules (+ (tally-rules t) (if parts (length rules) (max 0 (sub1 (length (header+rules ch))))))]
               [descended (+ (tally-descended t) (count values rules))]
               [lists (+ (tally-lists t) lists)]
               [lists-descended (+ (tally-lists-descended t) lists-descended)]
               [disagreements (append list-disagreements
                                      (if (equal? production reference) (tally-disagreements t)
                                          (cons (list 'entity label (map type-of ch) production reference) (tally-disagreements t))))]))

(define (report label t)
  (printf "  ~a: ~a texts, ~a entities (~a read by the grammar, ~a by descent whole), ~a rules (~a by descent), ~a token-list shapes (~a by descent), ~a disagreements\n"
          label (tally-docs t) (tally-entities t) (tally-brag-ok t) (tally-whole t) (tally-rules t) (tally-descended t)
          (tally-lists t) (tally-lists-descended t) (length (tally-disagreements t)))
  (for ([d (in-list (take (tally-disagreements t) (min 3 (length (tally-disagreements t)))))])
    (printf "    ~s\n" d)))

;; ---- the corpora --------------------------------------------------------------------------------
(define (fsh-files dir)
  (sort (for/list ([f (in-directory dir)] #:when (regexp-match? #rx"[.]fsh$" (path->string f))) (path->string f)) string<?))
(define (blocks file)
  (map cadr (regexp-match* #px"```fsh\n(.*?)```" (file->string file) #:match-select values)))
;; left-align : String -> String   as tests/sushi-importer-test.rkt reads SUSHI's indented inputs
(define (left-align text)
  (define lines (string-split text "\n" #:trim? #f))
  (define indents (for/list ([l (in-list lines)] #:unless (string=? (string-trim l) ""))
                    (string-length (car (regexp-match #px"^[ \t]*" l)))))
  (define n (if (null? indents) 0 (apply min indents)))
  (string-join (for/list ([l (in-list lines)]) (if (>= (string-length l) n) (substring l n) (string-trim l))) "\n"))

(test-case "the corpora: every vendored guide, the golden corpus, the importer texts, the guides' blocks"
  (printf "descent against brag\n")
  (define igs (for/list ([d (in-list '("ips-2.0.1" "mcode-4.0.0" "genomics-reporting-2.0.0" "plannet-1.2.0" "hrex-1.1.0" "golden"))])
                (define t (compare (map file->string (fsh-files (build-path vendor d)))))
                (report d t)
                (check-equal? (tally-disagreements t) '() d)
                t))
  ;; every entity of the guides is FSH, and descent reads nearly every rule of them itself: what it
  ;; leaves are the rules the grammar reads two ways (a ValueSet's or a RuleSet's `* ^x = v` and
  ;; `* insert R`)
  (for ([t (in-list igs)])
    (check-equal? (tally-brag-ok t) (tally-entities t))
    (check-true (>= (tally-descended t) (* 0.9 (tally-rules t)))))
  (check-true (>= (apply + (map tally-descended igs)) 23000) "descent reads the guides' rules itself")
  (define cases (call-with-input-file (build-path vendor "sushi-importer" "cases.json") read-json))
  (define importer (compare (append* (for/list ([c (in-list cases)]) (let ([s (hash-ref c 'input)]) (list s (left-align s)))))))
  (report "SUSHI importer texts" importer)
  (check-equal? (tally-disagreements importer) '())
  (check-true (> (tally-whole importer) 500))
  (check-true (< (tally-brag-ok importer) (tally-entities importer)) "the importer texts hold errors")
  (define guides (compare (append (blocks (build-path here 'up "guide.md")) (blocks (build-path here 'up "ig-guide.md")))))
  (report "guide.md and ig-guide.md" guides)
  (check-equal? (tally-disagreements guides) '())
  (check-equal? (tally-whole guides) (tally-entities guides)))

;; ---- every value in every place -----------------------------------------------------------------
;; The grammar reads some values two ways (a NUMBER is also a name, `true` a bool or a name, a code
;; also a quantity): each value form, in each rule a value ends, in each entity that has the rule.
(define value-forms
  '("\"text\"" "\"\"\"multi\nline\"\"\"" "3" "-1.5" "2020-01-01" "2020-01-01T10:00:00Z" "10:00" "10:00:30.5Z"
    "Reference(Patient/x)" "Reference(Patient/x) \"display\"" "Canonical(Foo)" "Canonical(Foo|1.0)"
    "#code" "#code \"Display\"" "$sct#123" "http://loinc.org#1234-5 \"LOINC\"" "#\"quoted code\""
    "5 'mg'" "'mg'" "'mg' \"milligram\"" "5 'mg' \"milligram\"" "3 #kg" "3 #kg \"kilogram\"" "#kg \"d\""
    "1 : 2" "1 'mg' : 2 'mg'" "1 : 'mg'" "#a : 3" "'mg' : 3 #kg \"k\"" "1 : 2 : 3"
    "true" "false" "SomeName" "and" "contains" "MS" "D" "system" "valueset"
    "" "\"a\" \"b\"" "3 \"x\"" "Reference(x) Reference(y)" "#a #b" "1 'mg' 'g'"))
(define value-places   ; (entity header, rule text before the value, takes `(exactly)`?)
  '(("Instance: i\nInstanceOf: Patient" "* active = " #t)
    ("Invariant: inv-1\nSeverity: #error" "* expression = " #t)
    ("Profile: P\nParent: Patient" "* active = " #t)
    ("Profile: P\nParent: Patient" "* ^status = " #f)
    ("Profile: P\nParent: Patient" "* name ^short = " #f)
    ("Extension: E" "* value[x] = " #t)
    ("Logical: L" "* ^abstract = " #f)
    ("Resource: R" "* x = " #t)
    ("ValueSet: V" "* ^status = " #f)
    ("ValueSet: V\nId: v\nTitle: \"V\"" "* ^experimental = " #f)
    ("ValueSet: V" "* #a ^designation.value = " #f)
    ("CodeSystem: C" "* ^caseSensitive = " #f)
    ("CodeSystem: C" "* #a #b ^property[0].valueCode = " #f)
    ("RuleSet: R" "* x = " #t)
    ("RuleSet: R" "* ^status = " #f)
    ("RuleSet: R" "* x ^short = " #f)
    ("RuleSet: R" "* #c ^display = " #f)))

(test-case "every value form in every place a value stands"
  (define texts
    (for*/list ([place (in-list value-places)] [v (in-list value-forms)] [exactly (in-list (if (caddr place) '("" " (exactly)") '("")))])
      (string-append (car place) "\n" (cadr place) v exactly "\n")))
  (define t (compare texts))
  (report "values" t)
  (check-equal? (tally-disagreements t) '())
  (check-true (> (tally-descended t) 600) "descent reads the values itself"))

;; ---- generated entities -------------------------------------------------------------------------
;; gen-entity : PRNG -> String   an entity grown from the grammar's productions — every kind of
;; entity, metadata and rule, keywords as names and paths, indented rules — and sometimes a rule its
;; entity does not allow
(define (gen-entity r)
  (define (pick . xs) (list-ref xs (random (length xs) r)))
  (define (maybe p s) (if (< (random r) p) s ""))
  (define (some n f) (for/list ([k (add1 (random n r))]) (f)))
  (define keywords '("and" "or" "from" "contains" "named" "only" "obeys" "true" "false" "include" "exclude"
                     "codes" "where" "valueset" "system" "contentReference" "MS" "SU" "TU" "N" "D"))
  (define (kw) (list-ref keywords (random (length keywords) r)))
  (define (name) (if (zero? (random 6 r)) (kw) (pick "Name" "my-thing" "$alias" "3" "2020-01-01" "10:00" "Patient")))
  (define (path) (if (zero? (random 6 r)) (kw) (pick "name" "value[x]" "extension[foo].valueString" "component[0].code" "identifier[+].system" ".")))
  (define (value) (list-ref value-forms (random (- (length value-forms) 6) r)))
  (define (card) (pick "0..1" "1..*" "..3" "1.." "0..0"))
  (define (flags) (string-join (for/list ([k (random 3 r)]) (pick "MS" "SU" "?!" "TU" "N" "D")) " "))
  (define (code) (pick "#a" "$sct#123" "#\"quoted code\"" "http://x.org#b"))
  (define (codes n) (string-join (for/list ([k n]) (code)) " "))
  (define (str) (pick "\"short\"" "\"a \\\"quoted\\\" word\"" "\"\"\"multi\nline\"\"\""))
  (define (caret) (pick "^short" "^extension[FMM].valueInteger" "^binding.description"))
  (define (ref) (pick "RS" "P(a, b)" "P([[x, y]])" "P(a)" "Q(\\) escaped, b)"))
  (define (target-type) (pick "Patient" "Reference(Patient or Group)" "Canonical(X)" "CodeableReference(Y)" (name)))
  (define (element-type) (pick "string" "Reference(Patient)" "Canonical(X)" "CodeableReference(Y)" "3" "boolean"))
  (define (joined sep f) (string-join (some 3 f) sep))
  (define (item) (string-append (name) (maybe 0.4 (string-append " named " (name))) " " (card) " " (flags)))
  (define (from)
    (pick (string-append "from system " (name) (maybe 0.4 (string-append " and valueset " (joined " and " name))))
          (string-append "from valueset " (joined " and " name) (maybe 0.3 (string-append " and system " (name))))))
  (define (filter-def)
    (string-append (name) " " (pick "=" "is-a" "descendent-of" "regex" "exists") " "
                   (pick (code) "true" "false" "/a.*b/" "\"str\"" (string-append (code) " \"disp\"") "")))
  (define rule-kinds
    (hash 'card (lambda () (string-append (path) " " (card) " " (flags)))
          'flag (lambda () (string-append (joined " and " path) " " (pick "MS" "SU" "?!") " " (flags)))
          'valueSet (lambda () (string-append (path) " from " (name) (maybe 0.6 (pick " (required)" " (extensible)" " (preferred)" " (example)"))))
          'fixedValue (lambda () (string-append (path) " = " (value) (maybe 0.2 " (exactly)")))
          'contains (lambda () (string-append (path) " contains " (joined " and " item)))
          'only (lambda () (string-append (path) " only " (joined " or " target-type)))
          'obeys (lambda () (string-append (maybe 0.6 (string-append (path) " ")) "obeys " (joined " and " name)))
          'caretValue (lambda () (string-append (maybe 0.5 (string-append (path) " ")) (caret) " = " (value)))
          'insert (lambda () (string-append (maybe 0.4 (string-append (path) " ")) "insert " (ref)))
          'path (lambda () (path))
          'addElement (lambda () (string-append (path) " " (card) " " (flags) " " (joined " or " element-type)
                                                (maybe 0.7 (string-append " " (str) (maybe 0.5 (string-append " " (str)))))))
          'addCRElement (lambda () (string-append (path) " " (card) " " (flags) " contentReference " (pick "http://x.org/SD#A.b" "#A.b")
                                                  (maybe 0.5 (string-append " \"short\"" (maybe 0.5 " \"def\"")))))
          'concept (lambda () (string-append (codes (add1 (random 2 r))) (maybe 0.6 (string-append " " (str) (maybe 0.5 (string-append " " (str)))))))
          'codeCaretValue (lambda () (string-append (codes (random 3 r)) " " (caret) " = " (value)))
          'codeInsert (lambda () (string-append (codes (random 3 r)) " insert " (ref)))
          'vsComponent (lambda () (string-append (pick "" "include " "exclude ")
                                                 (pick (string-append (code) (maybe 0.3 " \"disp\"") (maybe 0.5 (string-append " " (from))))
                                                       (string-append "codes " (from) (maybe 0.5 (string-append " where " (joined " and " filter-def)))))))
          'mapping (lambda () (string-append (maybe 0.6 (string-append (path) " ")) "-> \"target\"" (maybe 0.5 " \"comment\"") (maybe 0.4 " #code")))))
  (define sd '(card flag valueSet fixedValue contains only obeys caretValue insert path))
  (define-values (header allowed)
    (case (random 11 r)
      [(0) (values (string-append "Alias: " (pick "$a" "LNC") " = " (pick "http://loinc.org" "#c")) '())]
      [(1) (values (string-append "Profile: " (name) "\nParent: " (name) (maybe 0.5 (string-append "\nId: " (name))) (maybe 0.4 "\nTitle: \"T\"")
                                  (maybe 0.4 (string-append "\nDescription: " (str))))
                   sd)]
      [(2) (values (string-append "Extension: " (name) (maybe 0.5 "\nContext: Observation, \"Patient.name\"") (maybe 0.3 "\nId: e"))
                   sd)]
      [(3) (values (string-append "Logical: " (name) (maybe 0.5 "\nCharacteristics: #can-bind, #has-range") (maybe 0.3 "\nParent: Base"))
                   (append sd '(addElement addCRElement)))]
      [(4) (values (string-append "Resource: " (name) (maybe 0.5 "\nParent: DomainResource")) (append sd '(addElement addCRElement)))]
      [(5) (values (string-append "Instance: " (name) (maybe 0.8 (string-append "\nInstanceOf: " (name))) (maybe 0.3 "\nUsage: #example")
                                  (maybe 0.3 "\nTitle: \"T\""))
                   '(fixedValue insert path))]
      [(6) (values (string-append "Invariant: inv-1" (maybe 0.6 "\nDescription: \"d\"") (maybe 0.6 "\nExpression: \"a.exists()\"")
                                  (maybe 0.3 "\nXPath: \"f:x\"") (maybe 0.6 "\nSeverity: #error"))
                   '(fixedValue insert path))]
      [(7) (values (string-append "ValueSet: " (name) (maybe 0.4 "\nId: v") (maybe 0.4 "\nTitle: \"T\""))
                   '(vsComponent caretValue codeCaretValue insert codeInsert))]
      [(8) (values (string-append "CodeSystem: " (name) (maybe 0.4 "\nId: c")) '(concept codeCaretValue codeInsert))]
      [(9) (values (string-append "RuleSet: " (pick "RS" "Common")) (append sd '(addElement addCRElement concept codeCaretValue codeInsert vsComponent mapping)))]
      [else (values (string-append "Mapping: " (name) (maybe 0.5 "\nSource: P") (maybe 0.5 "\nTarget: \"http://x\"") (maybe 0.3 "\nId: m"))
                    '(mapping insert path))]))
  (define all-kinds (hash-keys rule-kinds))
  (define rules
    (for/list ([k (random (if (null? allowed) 2 6) r)])
      (define kind (if (or (null? allowed) (zero? (random 10 r))) (list-ref (sort all-kinds symbol<?) (random (length all-kinds) r))
                       (list-ref allowed (random (length allowed) r))))
      (string-append (if (zero? (random 8 r)) "  * " "* ") ((hash-ref rule-kinds kind)))))
  (string-append header "\n" (string-join rules "\n") "\n"))

;; broken : PRNG String -> String   one word dropped, doubled, or replaced by another kind of token
(define (broken r s)
  (define words (string-split s " "))
  (define k (random (length words) r))
  (define (pick . xs) (list-ref xs (random (length xs) r)))
  (string-join (append (take words k)
                       (case (random 3 r)
                         [(0) '()]
                         [(1) (list (list-ref words k) (list-ref words k))]
                         [else (list (pick "=" "from" "*" "->" "(required)" "#x" "^short" "and" "1..1" "\"s\"" ":"))])
                       (drop words (add1 k)))
               " "))

(test-case "generated entities: descent reads what brag reads, with brag's datum"
  (define r (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator r]) (random-seed 20261114))
  (define texts (for/list ([k 2000])
                  (define e (gen-entity r))
                  (if (zero? (random 4 r)) (broken r e) e)))
  (define t (compare texts))
  (report "generated" t)
  (check-equal? (tally-disagreements t) '())
  ;; both kinds were met, in numbers: FSH, read by descent, and text that is not
  (check-true (> (tally-brag-ok t) 1000) "the generator writes mostly FSH")
  (check-true (> (tally-whole t) 700) "descent reads most generated entities whole")
  (check-true (> (- (tally-entities t) (tally-brag-ok t)) 300) "the generator writes errors too"))

;; ---- token soups --------------------------------------------------------------------------------
;; soup : PRNG -> (listof Token)   an entity keyword, then tokens of any kind, values as the lexer
;; gives them
(define soup-tokens
  (list (cons 'SEQUENCE "a") (cons 'SEQUENCE "b") (cons 'NUMBER 3) (cons 'DATETIME "2020") (cons 'TIME "10:00")
        (cons 'STRING "s") (cons 'MULTILINE_STRING "m") (cons 'CODE (code-tok "#c")) (cons 'UNIT "mg")
        (cons 'CARD "0..1") (cons 'CARET_SEQUENCE "^x") (cons 'EQUAL "=") (cons 'COLON ":") (cons 'ARROW "->")
        (cons 'STAR "\n* ") (cons 'STAR "\n* ") (cons 'STAR "\n* ")
        (cons 'REFERENCE "Reference(x)") (cons 'CANONICAL "Canonical(x)") (cons 'CODEABLE_REFERENCE "CodeableReference(x)")
        (cons 'REGEX "/r/") (cons 'KW_EXACTLY "(exactly)") (cons 'KW_REQUIRED "(required)") (cons 'KW_MOD "?!")
        (cons 'KW_MS "MS") (cons 'KW_AND "and") (cons 'KW_OR "or") (cons 'KW_FROM "from") (cons 'KW_CONTAINS "contains")
        (cons 'KW_NAMED "named") (cons 'KW_ONLY "only") (cons 'KW_OBEYS "obeys") (cons 'KW_TRUE "true")
        (cons 'KW_INCLUDE "include") (cons 'KW_CODES "codes") (cons 'KW_WHERE "where") (cons 'KW_VSREFERENCE "valueset")
        (cons 'KW_SYSTEM "system") (cons 'KW_CONTENTREFERENCE "contentReference") (cons 'KW_INSERT "insert")
        (cons 'RULESET_REFERENCE "R") (cons 'PARAM_RULESET_REFERENCE "P") (cons 'PLAIN_PARAM "a,") (cons 'LAST_PLAIN_PARAM "b)")
        (cons 'KW_PARENT "Parent:") (cons 'KW_ID "Id:") (cons 'KW_TITLE "Title:") (cons 'KW_INSTANCEOF "InstanceOf:")
        (cons 'KW_SEVERITY "Severity:") (cons 'KW_CONTEXT "Context:") (cons 'LAST_QUOTED_CONTEXT "\"x\"")
        (cons 'KW_CHARACTERISTICS "Characteristics:") (cons 'LAST_CODE_ITEM "#b")))
(define (soup r)
  (define (tok p) (token (car p) (cdr p)))
  (cons (token (list-ref entity-start-types (random (length entity-start-types) r)) "Kw:")
        (for/list ([k (random 12 r)]) (tok (list-ref soup-tokens (random (length soup-tokens) r))))))

(test-case "token soups: descent takes nothing brag refuses"
  (define r (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator r]) (random-seed 1114))
  (define t (for/fold ([t empty-tally]) ([k 4000]) (compare-entity (soup r) t 'soup)))
  (report "token soups" t)
  (check-equal? (tally-disagreements t) '())
  (check-true (> (- (tally-entities t) (tally-brag-ok t)) 3000) "soups are mostly not FSH"))
