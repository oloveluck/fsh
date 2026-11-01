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
         (only-in "ast.rkt" code-tok))

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
(define (trim-multiline s) (substring s 3 (- (string-length s) 3)))
(define (unescape s)
  (regexp-replace* #px"\\\\(.)" s
    (lambda (_ c) (case c [("n") "\n"] [("t") "\t"] [("r") "\r"]
                          [("\"") "\""] [("\\") "\\"] [else c]))))
;; trim surrounding whitespace and a trailing "(" (for RuleSet/insert references)
(define (clean-ref s)
  (string-trim (regexp-replace #px"\\(\\s*$" (string-trim s) "")))

;; -------------------------------------------------------------------
;; tokenize: fresh mode stack per call; lexers close over push!/pop!
;; -------------------------------------------------------------------
(define (tokenize ip)
  (port-count-lines! ip)
  (define modes (box '(default)))
  (define (push! m) (set-box! modes (cons m (unbox modes))))
  (define (pop!)    (set-box! modes (cdr (unbox modes))))

  ;; ===== DEFAULT mode =====
  (define default-lexer
    (lexer
     [(eof) (void)]
     [block-comment   (token 'COMMENT lexeme #:skip? #t)]
     ;; STAR: a rule line — newline (or line comment) + ws + '*' + space. Must precede WS.
     [(:: (:or nl line-comment) (:* hs) "*" (:or #\space #\u00A0)) (token 'STAR lexeme)]
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
     [(:: digit digit digit digit (:? (:: "-" hh (:? (:: "-" hh (:? (:: "T" time-frag)))))))
      (token 'DATETIME lexeme)]
     [(:: hh (:? (:: ":" hh (:? (:: ":" hh (:? (:: "." (:+ digit)))))))
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
    (lexer
     [(eof) (void)]
     [(:: (:* anyws) (:+ rsnonws) (:* anyws) "(")
      (begin (push! 'param) (token 'PARAM_RULESET_REFERENCE (clean-ref lexeme)))]
     [(:: (:* anyws) (:+ rsnonws))
      (begin (pop!) (token 'RULESET_REFERENCE (clean-ref lexeme)))]))

  ;; ===== PARAM_RULESET_OR_INSERT mode =====
  (define param-lexer
    (lexer
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
    (lexer
     [(eof) (void)]
     [(:+ anyws) (token 'WS lexeme #:skip? #t)]
     [(:: "\"" str-body "\"" (:* anyws) ",") (token 'QUOTED_CONTEXT lexeme)]
     [(:: "\"" str-body "\"")          (begin (pop!) (token 'LAST_QUOTED_CONTEXT lexeme))]
     [(:: (:+ nonws) (:* anyws) ",")   (token 'UNQUOTED_CONTEXT lexeme)]
     [(:+ nonws)                       (begin (pop!) (token 'LAST_UNQUOTED_CONTEXT lexeme))]))

  ;; ===== LIST_OF_CODES mode =====
  (define codes-lexer
    (lexer
     [(eof) (void)]
     [(:+ anyws) (token 'WS lexeme #:skip? #t)]
     [(:: (:+ nonws) (:* anyws) ",") (token 'CODE_ITEM lexeme)]
     [(:+ nonws)                     (begin (pop!) (token 'LAST_CODE_ITEM lexeme))]))

  (lambda ()
    ((case (car (unbox modes))
       [(default)  default-lexer]
       [(ruleset)  ruleset-lexer]
       [(param)    param-lexer]
       [(contexts) contexts-lexer]
       [(codes)    codes-lexer])
     ip)))
