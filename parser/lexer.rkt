#lang racket/base

;;; lexer.rkt — a stateful tokenizer for FHIR Shorthand, ported from FSHLexer.g4.
;;;
;;; FSH is line-oriented and the ANTLR lexer uses pushMode/popMode. parser-tools/lex has no
;;; modes, so `tokenize` keeps a local mode-stack box and one `lexer` per ANTLR mode; the
;;; returned thunk dispatches on the stack top, and lexer actions mutate the stack (push/pop).
;;; Named tokens carry symbol types matching the brag grammar's terminals (KW_PROFILE,
;;; STRING, SEQUENCE, …). Structured tokens are ordered before SEQUENCE so same-length ties
;;; resolve in their favor (matching ANTLR's longest-match + first-rule-wins).

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         racket/string
         racket/contract
         (only-in racket/list last)
         (only-in "ast.rkt" code-tok)
         (only-in fhir/diagnostic diagnostic loc raise-diagnostic exn:fail:diagnostic?))

(provide
 (contract-out
  [tokenize (-> input-port? any/c)]))   ; FSH source port -> a brag token thunk

;; ---- shared character-class / fragment abbreviations ----------------
(define-lex-abbrevs
  [nl       (:or "\r\n" "\n" "\r")]
  [hs       (:or #\space #\tab #\page #\u00A0)]      ; horizontal space (incl nbsp)
  [anyws    (:or nl hs)]
  [nonws    (:~ #\space #\tab #\return #\newline #\page #\u00A0)]
  [digit    (:/ "0" "9")]
  [line-comment (:: "//" (:* (:~ #\newline #\return)) (:? nl))]
  [block-comment (:: "/*" (:* (:or (:~ #\*) (:: #\* (:~ #\/)))) (:* #\*) "*/")]
  [str-body (:* (:or (:: "\\" any-char) (:~ #\" #\\)))]
  [ml-body  (:* (:or (:~ #\") (:: #\" (:~ #\")) (:: #\" #\" (:~ #\"))))]
  [hh       (:: digit digit)]
  [time-frag (:: hh (:? (:: ":" hh (:? (:: ":" hh (:? (:: "." (:+ digit)))))))
                 (:? (:or "Z" (:: (:or "+" "-") hh ":" hh))))]
  [rsnonws  (:~ #\space #\tab #\return #\newline #\page #\u00A0 #\()])

;; ---- value helpers --------------------------------------------------
(define (trim-quotes s) (substring s 1 (sub1 (string-length s))))
;; a triple-quoted string: the body without the delimiters, a whitespace-only first and last line
;; dropped, and the common indentation of the remaining lines removed (SUSHI's semantics)
(define (trim-multiline s)
  (define body (substring s 3 (- (string-length s) 3)))
  (define lines (regexp-split #rx"\r?\n" body))
  (define (blank? l) (regexp-match? #px"^[ \t]*$" l))
  (define trimmed
    (let* ([ls (if (and (pair? lines) (pair? (cdr lines)) (blank? (car lines))) (cdr lines) lines)]
           [ls (if (and (pair? ls) (pair? (cdr ls)) (blank? (last ls))) (reverse (cdr (reverse ls))) ls)])
      ls))
  (define indent
    (for/fold ([m #f]) ([l (in-list trimmed)] #:unless (blank? l))
      (define n (string-length (car (regexp-match #px"^[ \t]*" l))))
      (if m (min m n) n)))
  (string-join (for/list ([l (in-list trimmed)])
                 (cond [(blank? l) ""]
                       [(and indent (>= (string-length l) indent)) (substring l indent)]
                       [else l]))
               "\n"))
(define (unescape s)
  (regexp-replace* #px"\\\\(.)" s
    ;; only the escapes FSH defines; any other `\x` is kept verbatim (markdown `\|` in descriptions)
    (lambda (all c) (case c [("n") "\n"] [("t") "\t"] [("r") "\r"]
                          [("\"") "\""] [("\\") "\\"] [else all]))))
;; trim surrounding whitespace and a trailing "(" (for RuleSet/insert references)
(define (clean-ref s)
  (string-trim (regexp-replace #px"\\(\\s*$" (string-trim s) "")))

;; -------------------------------------------------------------------
;; tokenize: fresh mode stack per call; lexers close over push!/pop!
;; -------------------------------------------------------------------
(define (tokenize ip)
  (port-count-lines! ip)
  ;; positioned : lexer-srcloc result -> token   the token with its line/column/position/span
  ;; (brag reports parse errors at them; the token accessors downstream are unchanged)
  (define (positioned t)
    (cond [(srcloc-token? t)
           (define tok (srcloc-token-token t))
           (define sl (srcloc-token-srcloc t))
           (if (token-struct? tok)
               (token (token-struct-type tok) (token-struct-val tok)
                      #:line (srcloc-line sl) #:column (srcloc-column sl)
                      #:position (srcloc-position sl) #:span (srcloc-span sl)
                      #:skip? (token-struct-skip? tok))
               tok)]
          [else t]))
  (define modes (box '(default)))
  (define (push! m) (set-box! modes (cons m (unbox modes))))
  (define (pop!)    (set-box! modes (cdr (unbox modes))))

  ;; ===== DEFAULT mode =====
  (define default-lexer
    (lexer-srcloc
     [(eof) (void)]
     [block-comment   (token 'COMMENT lexeme #:skip? #t)]
     ;; STAR: a rule line — newline (or line comment) + ws + '*' + space. Must precede WS.
     ;; Trailing spaces before the newline and BLANK lines after it belong to the STAR too: the WS
     ;; rule below is greedy, so a bare `(:* hs)` here let WS swallow " \n" / "\n\n" (newline
     ;; included) and strand the `*` as a SEQUENCE — any rule after a line with trailing whitespace,
     ;; or after a blank line, failed to parse. Longest match now always prefers STAR.
     [(:: (:* hs) (:or nl line-comment) (:* anyws) "*" (:or #\space #\u00A0)) (token 'STAR lexeme)]
     [line-comment    (token 'COMMENT lexeme #:skip? #t)]
     [(:+ anyws)      (token 'WS lexeme #:skip? #t)]

     ;; strings (multiline before string)
     [(:: "\"\"\"" ml-body "\"\"\"") (token 'MULTILINE_STRING (trim-multiline lexeme))]
     [(:: "\"" str-body "\"")        (token 'STRING (unescape (trim-quotes lexeme)))]

     ;; keyword-with-colon declarations (single tokens; longer than SEQUENCE for "Profile:")
     [(:: "Alias"           (:* anyws) ":") (token 'KW_ALIAS lexeme)]
     [(:: "Profile"         (:* anyws) ":") (token 'KW_PROFILE lexeme)]
     [(:: "Extension"       (:* anyws) ":") (token 'KW_EXTENSION lexeme)]
     [(:: "InstanceOf"      (:* anyws) ":") (token 'KW_INSTANCEOF lexeme)]
     [(:: "Instance"        (:* anyws) ":") (token 'KW_INSTANCE lexeme)]
     [(:: "Invariant"       (:* anyws) ":") (token 'KW_INVARIANT lexeme)]
     [(:: "ValueSet"        (:* anyws) ":") (token 'KW_VALUESET lexeme)]
     [(:: "CodeSystem"      (:* anyws) ":") (token 'KW_CODESYSTEM lexeme)]
     [(:: "Mapping"         (:* anyws) ":") (token 'KW_MAPPING lexeme)]
     [(:: "Logical"         (:* anyws) ":") (token 'KW_LOGICAL lexeme)]
     [(:: "Resource"        (:* anyws) ":") (token 'KW_RESOURCE lexeme)]
     [(:: "Parent"          (:* anyws) ":") (token 'KW_PARENT lexeme)]
     [(:: "Id"              (:* anyws) ":") (token 'KW_ID lexeme)]
     [(:: "Title"           (:* anyws) ":") (token 'KW_TITLE lexeme)]
     [(:: "Description"     (:* anyws) ":") (token 'KW_DESCRIPTION lexeme)]
     [(:: "Expression"      (:* anyws) ":") (token 'KW_EXPRESSION lexeme)]
     [(:: "XPath"           (:* anyws) ":") (token 'KW_XPATH lexeme)]
     [(:: "Severity"        (:* anyws) ":") (token 'KW_SEVERITY lexeme)]
     [(:: "Usage"           (:* anyws) ":") (token 'KW_USAGE lexeme)]
     [(:: "Source"          (:* anyws) ":") (token 'KW_SOURCE lexeme)]
     [(:: "Target"          (:* anyws) ":") (token 'KW_TARGET lexeme)]
     ;; mode-pushing declarations
     [(:: "RuleSet"         (:* anyws) ":") (begin (push! 'ruleset) (token 'KW_RULESET lexeme))]
     [(:: "Context"         (:* anyws) ":") (begin (push! 'contexts) (token 'KW_CONTEXT lexeme))]
     [(:: "Characteristics" (:* anyws) ":") (begin (push! 'codes) (token 'KW_CHARACTERISTICS lexeme))]

     ;; strength / exactly
     [(:: "(" (:* anyws) "example"    (:* anyws) ")") (token 'KW_EXAMPLE lexeme)]
     [(:: "(" (:* anyws) "preferred"  (:* anyws) ")") (token 'KW_PREFERRED lexeme)]
     [(:: "(" (:* anyws) "extensible" (:* anyws) ")") (token 'KW_EXTENSIBLE lexeme)]
     [(:: "(" (:* anyws) "required"   (:* anyws) ")") (token 'KW_REQUIRED lexeme)]
     [(:: "(" (:* anyws) "exactly"    (:* anyws) ")") (token 'KW_EXACTLY lexeme)]
     ["?!" (token 'KW_MOD lexeme)]

     ;; word keywords (before SEQUENCE; whitespace-delimited so won't split paths)
     ["MS" (token 'KW_MS lexeme)] ["SU" (token 'KW_SU lexeme)] ["TU" (token 'KW_TU lexeme)]
     ["N" (token 'KW_NORMATIVE lexeme)] ["D" (token 'KW_DRAFT lexeme)]
     ["from" (token 'KW_FROM lexeme)] ["contains" (token 'KW_CONTAINS lexeme)]
     ["named" (token 'KW_NAMED lexeme)] ["and" (token 'KW_AND lexeme)]
     ["only" (token 'KW_ONLY lexeme)] ["or" (token 'KW_OR lexeme)]
     ["obeys" (token 'KW_OBEYS lexeme)] ["true" (token 'KW_TRUE lexeme)]
     ["false" (token 'KW_FALSE lexeme)] ["include" (token 'KW_INCLUDE lexeme)]
     ["exclude" (token 'KW_EXCLUDE lexeme)] ["codes" (token 'KW_CODES lexeme)]
     ["where" (token 'KW_WHERE lexeme)] ["valueset" (token 'KW_VSREFERENCE lexeme)]
     ["system" (token 'KW_SYSTEM lexeme)] ["contentReference" (token 'KW_CONTENTREFERENCE lexeme)]
     ["insert" (begin (push! 'ruleset) (token 'KW_INSERT lexeme))]

     ;; typed forms (before SEQUENCE)
     [(:: "Reference"         (:* anyws) "(" (:* (:~ #\))) ")") (token 'REFERENCE lexeme)]
     [(:: "CodeableReference" (:* anyws) "(" (:* (:~ #\))) ")") (token 'CODEABLE_REFERENCE lexeme)]
     [(:: "Canonical"         (:* anyws) "(" (:* (:~ #\))) ")") (token 'CANONICAL lexeme)]

     ;; literals / codes (all before SEQUENCE so same-length ties win)
     [(:: (:* (:~ #\space #\tab #\return #\newline #\page #\u00A0 #\#))
          "#" (:or (:: "\"" (:* (:or (:~ #\" #\\) "\\\"" "\\\\")) "\"") (:+ nonws)))
      (token 'CODE (code-tok lexeme))]
     [(:: (:? (:or "+" "-")) (:+ digit) "-" hh (:? (:: "-" hh (:? (:: "T" time-frag)))))
      (token 'DATETIME lexeme)]
     ;; a bare four-digit `6000` is a NUMBER (FSHLexer.g4 declares NUMBER first, so it wins the tie);
     ;; as a DATETIME every four-digit quantity (`6000 'cGy'`) failed to parse. A year assigned to a
     ;; date element still compiles to "2020" (compile.rkt's coerce writes numbers as strings there)
     [(:: digit digit digit digit (:: "-" hh (:? (:: "-" hh (:? (:: "T" time-frag))))))
      (token 'DATETIME lexeme)]
     ;; a TIME needs at least hh:mm — a bare two-digit `40` is a NUMBER (as in FSHLexer.g4, where
     ;; NUMBER wins the tie); otherwise every two-digit quantity (`40 'mg'`) failed to parse
     [(:: hh ":" hh (:? (:: ":" hh (:? (:: "." (:+ digit)))))
          (:? (:or "Z" (:: (:or "+" "-") hh ":" hh))))
      (token 'TIME lexeme)]
     [(:: (:? (:or "+" "-")) (:+ digit) (:? (:: "." (:+ digit)))
          (:? (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit))))
      (token 'NUMBER (string->number lexeme))]
     [(:: (:? (:+ digit)) ".." (:? (:or (:+ digit) "*"))) (token 'CARD lexeme)]
     [(:: "'" (:* (:~ #\' #\\)) "'") (token 'UNIT (trim-quotes lexeme))]
     [(:: "^" (:+ nonws)) (token 'CARET_SEQUENCE lexeme)]
     [(:: "/" (:or "\\/" (:~ #\* #\/ #\return #\newline))
          (:* (:or "\\/" (:~ #\/ #\return #\newline))) "/")
      (token 'REGEX lexeme)]

     ;; punctuation
     ["->" (token 'ARROW lexeme)]
     ["="  (token 'EQUAL lexeme)]
     [":"  (token 'COLON lexeme)]
     [","  (token 'COMMA lexeme)]

     ;; catch-all non-whitespace
     [(:+ nonws) (token 'SEQUENCE lexeme)]))

  ;; ===== RULESET_OR_INSERT mode =====
  (define ruleset-lexer
    (lexer-srcloc
     [(eof) (void)]
     [(:: (:* anyws) (:+ rsnonws) (:* anyws) "(")
      (begin (push! 'param) (token 'PARAM_RULESET_REFERENCE (clean-ref lexeme)))]
     [(:: (:* anyws) (:+ rsnonws))
      (begin (pop!) (token 'RULESET_REFERENCE (clean-ref lexeme)))]))

  ;; ===== PARAM_RULESET_OR_INSERT mode =====
  (define param-lexer
    (lexer-srcloc
     [(eof) (void)]
     [(:: (:* anyws) "[[" (:* (:or (:~ #\]) (:: #\] (:~ #\])))) "]]" (:* anyws) ")")
      (begin (pop!) (pop!) (token 'LAST_BRACKETED_PARAM lexeme))]
     [(:: (:* anyws) "[[" (:* (:or (:~ #\]) (:: #\] (:~ #\])))) "]]" (:* anyws) ",")
      (token 'BRACKETED_PARAM lexeme)]
     [(:: (:* anyws) (:* (:or "\\)" "\\," "\\\\" (:~ #\) #\,))) (:* anyws) ")")
      (begin (pop!) (pop!) (token 'LAST_PLAIN_PARAM lexeme))]
     [(:: (:* anyws) (:* (:or "\\)" "\\," "\\\\" (:~ #\) #\,))) (:* anyws) ",")
      (token 'PLAIN_PARAM lexeme)]))

  ;; ===== LIST_OF_CONTEXTS mode =====
  (define contexts-lexer
    (lexer-srcloc
     [(eof) (void)]
     [(:+ anyws) (token 'WS lexeme #:skip? #t)]
     [(:: "\"" str-body "\"" (:* anyws) ",") (token 'QUOTED_CONTEXT lexeme)]
     [(:: "\"" str-body "\"")          (begin (pop!) (token 'LAST_QUOTED_CONTEXT lexeme))]
     [(:: (:+ nonws) (:* anyws) ",")   (token 'UNQUOTED_CONTEXT lexeme)]
     [(:+ nonws)                       (begin (pop!) (token 'LAST_UNQUOTED_CONTEXT lexeme))]))

  ;; ===== LIST_OF_CODES mode =====
  (define codes-lexer
    (lexer-srcloc
     [(eof) (void)]
     [(:+ anyws) (token 'WS lexeme #:skip? #t)]
     [(:: (:+ nonws) (:* anyws) ",") (token 'CODE_ITEM lexeme)]
     [(:+ nonws)                     (begin (pop!) (token 'LAST_CODE_ITEM lexeme))]))

  (lambda ()
    (define-values (line col pos) (port-next-location ip))
    ;; text no rule matches is a positioned LEX error
    (with-handlers ([(lambda (e) (and (exn:fail? e) (not (exn:fail:diagnostic? e))))
                     (lambda (e)
                       (define rest (let ([s (peek-string 30 0 ip)]) (if (eof-object? s) "" (car (string-split (string-append s " ") "\n")))))
                       (raise-diagnostic (diagnostic 'fsh 'lex 'error (format "unrecognized text `~a`" (string-trim rest))
                                                     (loc #f line (and col (add1 col)) pos #f))))])
      (positioned
       ((case (car (unbox modes))
          [(default)  default-lexer]
          [(ruleset)  ruleset-lexer]
          [(param)    param-lexer]
          [(contexts) contexts-lexer]
          [(codes)    codes-lexer])
        ip)))))
