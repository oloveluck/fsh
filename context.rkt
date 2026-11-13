#lang racket/base
;;; context.rkt — the compiler's context: the parameters every stage reads (the canonical base, the FHIR
;;; version, the document's aliases, profiles, slice orders and instance resolvers, rule diagnostics)
;;; and the small helpers over names, aliases, codes and canonicals. The bottom of the compiler.
(require racket/match
         racket/string
         (only-in racket/list last)
         "parser/ast.rkt"
         ;; for effect: FHIR's primitive types (string, dateTime, code …) are registered as types by
         ;; this module, not by a tower. The compiler asks `type-registered?` of every type name it
         ;; meets, and without them `* effective[x] only dateTime` was read as a PROFILE named dateTime
         ;; (type code dateTime, profile …/StructureDefinition/dateTime) — unless something else in the
         ;; process happened to have loaded them (fhirpath, the validator, fml)
         (only-in fhir/primitives/primitives)
         (only-in fhir/sd/sd-index resolve-canonical-raw sd-index-available?))
(provide assoc-ref canonical-base fsh-config-status fsh-config-version capitalize code-text* current-aliases current-contained current-instance-ref current-instance-resolver current-local-canonical current-logicals current-profile-sd current-profiles current-rule-diagnostics current-slice-orders emit-snapshot? external-canonical fsh-fhir-version last-url-seg resolve resolve-alias sd-url split-code strip#)
;; the IG canonical URL base (SUSHI default in the golden corpus is http://example.org)
(define canonical-base (make-parameter "http://example.org"))
(define fsh-fhir-version (make-parameter "5.0.0"))
;; fsh-config-status : (parameter (or String #f))   sushi-config.yaml's `status`, which SUSHI gives every
;; StructureDefinition, ValueSet and CodeSystem that sets none itself; #f (no config) means "active"
(define fsh-config-status (make-parameter #f))
;; fsh-config-version : (parameter (or String #f))   the `version` SUSHI stamps on the same resources —
;; only when the config says `FSHOnly: true` (otherwise the IG Publisher sets it); #f writes none
(define fsh-config-version (make-parameter #f))
;; opt-in: also emit a full snapshot (differential merged onto the base's snapshot). Off by default
;; (the golden corpus is differential-only); loads the R5 core package on first use.
(define emit-snapshot? (make-parameter #f))
(define current-rule-diagnostics (make-parameter #f))
;; external-canonical : kind name -> url | #f   a bare NAME (not a url) of a CodeSystem / ValueSet in
;; the core/dependency packages the SD index sees (`from system ConditionCategoryCodes`)
(define (external-canonical kind n)
  (and (string? n) (not (regexp-match? #rx"[:/]" n)) (sd-index-available?)
       (let ([r (resolve-canonical-raw kind n)]) (and r (hash-ref r 'url #f)))))
;; current-profile-sd : (parameter (string -> (or jsexpr #f)))   a local profile's compiled SD
(define current-profile-sd (make-parameter (lambda (_) #f)))
;; current-instance-resolver : (parameter (string -> (or jsexpr #f)))   name/id -> the compiled instance
(define current-instance-resolver (make-parameter (lambda (_) #f)))
;; current-local-canonical : (parameter (string -> (or url #f)))  this document's ValueSet/CodeSystem
;; names and ids -> canonical URLs
(define current-local-canonical (make-parameter (lambda (_) #f)))
;; current-aliases : the document's Alias table (set by compile-doc/errors) — carets resolve through it
(define current-aliases (make-parameter (hash)))
;; current-slice-orders : profile name/id -> (hash element-path -> slice names in declaration order)
(define current-slice-orders (make-parameter (lambda (_) (hash))))
;; current-profiles : the document's Profiles/Extensions by name/id/url -> (cons parent id)
(define current-profiles (make-parameter (hash)))
;; current-logicals : the document's Logical models and custom Resources by name/id/url -> canonical
;; url (a logical type is named by its url: as a Parent, as an element's type)
(define current-logicals (make-parameter (hash)))
;; ---- misc ----------------------------------------------------------------------------------
(define (assoc-ref alist key) (cond [(assq key alist) => cdr] [else #f]))
(define (resolve v aliases)
  (match v
    [(struct fsh-reference (t d)) (fsh-reference (resolve-reference (resolve-alias t aliases) aliases) d)]
    ;; `$sct#123` / `SCT#123`: the system part of a code is an alias, as in rules and value sets
    [(struct fsh-code (c d))
     (match (regexp-match #px"^([^#]+)#(.*)$" c)
       [(list _ sys code) (fsh-code (string-append (resolve-system sys aliases) "#" code) d)]
       [#f v])]
    ;; a quantity's coded unit (`1 SCT#428673006 "tablet"`): the system part is an alias too
    [(struct fsh-quantity (n u d))
     (match (and (string? u) (regexp-match #px"^([^#]+)#(.*)$" u))
       [(list _ sys code) (fsh-quantity n (string-append (resolve-system sys aliases) "#" code) d)]
       [_ v])]
    ;; Canonical(Name) / Canonical(Name|version): an alias, this document's profiles, value sets and
    ;; code systems, or a definition in the packages, as its canonical url (the version kept)
    [(struct fsh-canonical (t0))
     (define t (match (regexp-match #px"^\\s*Canonical\\s*\\(\\s*(.*?)\\s*\\)\\s*$" t0) [(list _ inner) inner] [#f t0]))
     (match-define (list _ n ver) (regexp-match #px"^([^|]*)(\\|.*)?$" t))
     (define r (resolve-alias (string-trim n) aliases))
     (define url (or (let ([p (hash-ref (current-profiles) r #f)])
                       (and p (string-append (canonical-base) "/StructureDefinition/" (cdr p))))
                     (hash-ref (current-logicals) r #f)
                     ((current-local-canonical) r)
                     (external-canonical "StructureDefinition" r)
                     (external-canonical "ValueSet" r)
                     (external-canonical "CodeSystem" r)
                     r))
     (fsh-canonical (string-append url (or ver "")))]
    [(? string? s) (resolve-alias s aliases)]
    [_ v]))
(define (resolve-alias s aliases) (if (and (string? s) (hash-has-key? aliases s)) (hash-ref aliases s) s))
;; resolve-system : system-part aliases -> url   the system of a code: an alias, else the NAME of a
;; CodeSystem of this document (`LanguageProficiencyCS#30`) or of the packages, else as written
(define (resolve-system sys aliases)
  (cond [(hash-has-key? aliases sys) (hash-ref aliases sys)]
        [(regexp-match? #rx"[:/]" sys) sys]
        [else (or ((current-local-canonical) sys) (external-canonical "CodeSystem" sys) sys)]))
;; resolve-reference : "Reference(X)" | "X" -> the reference string. X naming an instance of this
;; document becomes "<ResourceType>/<id>" (SUSHI); anything else (Type/id, a URL, an alias) stays.
(define (resolve-reference t aliases)
  (define inner (match (regexp-match #px"^\\s*Reference\\s*\\(\\s*(.*?)\\s*\\)\\s*$" t)
                  [(list _ x) (resolve-alias x aliases)] [#f t]))
  (cond [(assoc inner (current-contained)) => (lambda (p) (string-append "#" (cdr p)))]   ; a contained one: local
        [else (or ((current-instance-ref) inner) inner)]))
;; current-instance-ref : (parameter (string -> (or "Type/id" #f)))
(define current-instance-ref (make-parameter (lambda (_) #f)))
;; current-contained : (parameter (listof (cons name id)))   the instances the one being built contains
(define current-contained (make-parameter '()))
(define (code-text* x) (if (code-tok? x) (code-tok-text x) x))
(define (strip# s) (if (and (string? s) (regexp-match? #px"^#" s)) (substring s 1) s))
(define (split-code c)
  (define m (regexp-match #px"^([^#]*)#(.*)$" c))
  (if m (values (let ([s (cadr m)]) (and (positive? (string-length s)) s)) (unquote-code (caddr m))) (values #f c)))
;; unquote-code : string -> string   a quoted concept code (`$PHARMVAR#"CYP2C9 *2/*5"`) is its text
(define (unquote-code s)
  (cond [(regexp-match #px"^\"(.*)\"$" s) => (lambda (m) (regexp-replace* #px"\\\\(.)" (cadr m) "\\1"))]
        [else s]))
(define (capitalize s)
  (if (= 0 (string-length s)) s (string-append (string-upcase (substring s 0 1)) (substring s 1))))
(define (sd-url t)
  (if (regexp-match? #px"^https?:" t) t (string-append "http://hl7.org/fhir/StructureDefinition/" t)))
(define (last-url-seg s) (if (regexp-match? #rx"/" s) (last (string-split s "/")) s))

(module+ test
  (require rackunit)
  ;; names, aliases, codes and canonicals
  (check-equal? (resolve-alias "$loinc" (hash "$loinc" "http://loinc.org")) "http://loinc.org")
  (check-equal? (resolve-alias "Patient" (hash)) "Patient")
  (check-equal? (strip# "#final") "final")
  (check-equal? (strip# "final") "final")
  (let-values ([(sys code) (split-code "http://loinc.org#85354-9")]) (check-equal? (list sys code) '("http://loinc.org" "85354-9")))
  (let-values ([(sys code) (split-code "#final")]) (check-equal? (list sys code) '(#f "final")))
  (check-equal? (capitalize "quantity") "Quantity")
  (check-equal? (sd-url "Observation") "http://hl7.org/fhir/StructureDefinition/Observation")
  (check-equal? (sd-url "http://example.org/StructureDefinition/X") "http://example.org/StructureDefinition/X")
  (check-equal? (last-url-seg "http://example.org/fhir/Patient") "Patient")
  (check-equal? (last-url-seg "Patient") "Patient")
  (check-equal? (code-text* (code-tok "x")) "x")
  (check-equal? (resolve-reference "Reference(Patient/1)" (hash)) "Patient/1")
  (check-equal? (resolve-reference "Patient/1" (hash)) "Patient/1"))
