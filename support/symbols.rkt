#lang racket/base

;;; support/symbols.rkt — what an editor asks about an FSH document besides its problems: the
;;; definition a name refers to, and what an element path means. Both read the text, not a parse: an
;;; entity is a `Keyword: Name` line (or the `Id:` under one), an element is found in the base type's
;;; snapshot through the SD index. Parser-free, so it works on a document that does not parse yet,
;;; and fast. The Entity and Position data definitions are fhir/lang/support's.

(require racket/contract racket/string racket/list racket/match
         (only-in fhir/sd/sd-index resolve-sd-raw sd-index-available?)
         (only-in fhir/lang/support entity entity? entity-kind entity-name entity-id entity-file entity-line
                  find-entity line-of))

(provide
 (contract-out
  [entities-of      (-> string? path-string? (listof entity?))]
  [rule-path-at     (-> string? pair? (or/c string? #f))]
  [parent-of        (-> string? (listof entity?) (hash/c string? string?) (or/c string? #f))]
  [element-hover    (-> string? string? (or/c string? #f))]
  [entity-hover     (-> entity? string? (or/c string? #f))]
  [aliases-of       (-> string? (hash/c string? string?))]
  [project-root     (-> path-string? path?)]
  [project-fsh-files (-> path? (listof path?))]))
(define entity-keywords '("Profile" "Extension" "Instance" "ValueSet" "CodeSystem" "Invariant" "RuleSet"
                          "Mapping" "Logical" "Resource" "Alias"))
(define header-rx (pregexp (string-append "^(" (string-join entity-keywords "|") "):\\s*([^\\s(=]+)")))

;; entities-of : text file -> entities   every declaration in the text, with its Id when given
(define (entities-of text file)
  (define lines (regexp-split #rx"\r?\n" text))
  (define heads
    (for/list ([l (in-list lines)] [i (in-naturals)] #:when (regexp-match? header-rx l))
      (match-define (list _ kind name) (regexp-match header-rx l))
      (list kind name i)))
  (for/list ([h (in-list heads)] [next (in-list (append (map caddr (cdr heads)) (list (length lines))))])
    (match-define (list kind name i) h)
    (define id (for/or ([l (in-list (take (drop lines i) (- next i)))])
                 (define m (regexp-match #px"^Id:\\s*(\\S+)" l))
                 (and m (cadr m))))
    (entity kind name id (if (path? file) (path->string file) file) i)))

;; aliases-of : text -> (hash alias -> value)   `Alias: $loinc = http://loinc.org`
(define (aliases-of text)
  (for/hash ([m (in-list (regexp-match* #px"(?m:^Alias:\\s*(\\S+)\\s*=\\s*(\\S+))" text #:match-select cdr))])
    (values (car m) (cadr m))))

;; project-root : file -> directory   where its sushi-config.yaml is (beside it or up to two
;; levels up, as fsh's compile-project looks), else the file's own directory
(define (project-root file)
  (define dir (simplify-path (path->complete-path (let-values ([(base name must-be-dir?) (split-path (if (path? file) file (string->path file)))])
                                                     (if (path? base) base (current-directory))))))
  (or (for/or ([d (list dir (build-path dir 'up) (build-path dir 'up 'up))])
        (and (file-exists? (build-path d "sushi-config.yaml")) (simplify-path d)))
      dir))
;; project-fsh-files : directory -> files   every .fsh under it
(define (project-fsh-files dir)
  (if (directory-exists? dir)
      (sort (for/list ([f (in-directory dir)] #:when (regexp-match? #rx"[.]fsh$" (path->string f))) f) string<? #:key path->string)
      '()))

;; rule-path-at : text position -> path|#f   the element path of the rule on the cursor's line
;; (`* component[systolic].code = …` -> "component[systolic].code"), when the cursor is on it
(define (rule-path-at text pos)
  (define line (line-of text (car pos)))
  (define m (and line (regexp-match-positions #px"^\\s*\\*\\s+([A-Za-z_][A-Za-z0-9_.\\[\\]:+=$/-]*)" line)))
  (and m (let ([span (cadr m)])
           (and (<= (car span) (cdr pos) (cdr span))
                (let ([p (substring line (car span) (cdr span))])
                  (and (not (regexp-match? #rx"^(insert|obeys|include|exclude|codes)$" p)) p))))))

;; parent-of : text entities aliases -> base type|#f   the document's Parent / InstanceOf, followed
;; through the project's own profiles (by name or id) to a FHIR type name
(define (parent-of text es aliases [fuel 16])
  (define m (regexp-match #px"(?m:^(?:Parent|InstanceOf):\\s*(\\S+))" text))
  (and m (resolve-type (cadr m) es aliases fuel)))
(define (resolve-type name es aliases fuel)
  (define n (hash-ref aliases name name))
  (cond
    [(<= fuel 0) #f]
    [(regexp-match? #rx"^https?://" n) (last (string-split n "/"))]
    [(find-entity es n)
     => (lambda (e)
          (cond [(member (entity-kind e) '("Profile" "Extension" "Logical" "Resource"))
                 ;; the parent's own Parent: read its file's text around the declaration
                 (define text (with-handlers ([exn:fail? (lambda (_) #f)]) (file->string* (entity-file e))))
                 (define lines (and text (drop (regexp-split #rx"\r?\n" text) (entity-line e))))
                 (define p (and lines (for/or ([l (in-list (take lines (min 12 (length lines))))])
                                        (define mm (regexp-match #px"^Parent:\\s*(\\S+)" l))
                                        (and mm (cadr mm)))))
                 (if p (resolve-type p es aliases (sub1 fuel)) (if (equal? (entity-kind e) "Extension") "Extension" #f))]
                [else #f]))]
    [else n]))
(define (file->string* f) (call-with-input-file f (lambda (in) (read-string 100000000 in))))

;; element-hover : base-type fsh-path -> markdown|#f   the element's definition from the base's
;; snapshot: slice brackets dropped, a choice name (valueQuantity) matched to its value[x]
(define (element-hover base-type fsh-path)
  (define sd (and (sd-index-available?) (resolve-sd-raw base-type)))
  (define els (and (hash? sd) (hash-ref (hash-ref sd 'snapshot (hasheq)) 'element '())))
  (and (list? els) (pair? els)
       (let* ([steps (map (lambda (s) (regexp-replace* #px"\\[[^\\]]*\\]" s "")) (string-split fsh-path "."))]
              [steps (if (and (pair? steps) (equal? (car steps) base-type)) (cdr steps) steps)]
              [path (string-join (cons base-type steps) ".")]
              [el (or (findf (lambda (e) (equal? (hash-ref e 'path #f) path)) els)
                      (findf (lambda (e) (choice-match? (hash-ref e 'path "") path)) els))])
         (and el
              (string-append
               (format "**~a** ~a..~a" (hash-ref el 'path) (hash-ref el 'min 0) (hash-ref el 'max "*"))
               (let ([ts (for/list ([t (in-list (hash-ref el 'type '()))]) (hash-ref t 'code ""))])
                 (if (pair? ts) (string-append "  `" (string-join ts " | ") "`") ""))
               (let ([s (hash-ref el 'short #f)]) (if s (string-append "\n\n" s) ""))
               (let ([d (hash-ref el 'definition #f)]) (if (and d (not (equal? d (hash-ref el 'short #f)))) (string-append "\n\n" d) "")))))))
;; choice-match? : element-path candidate -> boolean   "Observation.value[x]" matches "Observation.valueQuantity"
(define (choice-match? el-path cand)
  (and (regexp-match? #rx"\\[x\\]$" el-path)
       (let ([stem (substring el-path 0 (- (string-length el-path) 3))])
         (and (string-prefix? cand stem) (> (string-length cand) (string-length stem))
              (char-upper-case? (string-ref cand (string-length stem)))))))

;; entity-hover : entity text-of-its-file -> markdown   the declaration's header lines
(define (entity-hover e text)
  (define lines (drop (regexp-split #rx"\r?\n" text) (entity-line e)))
  (define header (let loop ([ls lines] [acc '()])
                   (if (or (null? ls) (and (pair? acc) (or (string=? (string-trim (car ls)) "") (regexp-match? #rx"^\\*" (car ls)))))
                       (reverse acc)
                       (loop (cdr ls) (cons (car ls) acc)))))
  (string-append "```fsh\n" (string-join (take header (min 8 (length header))) "\n") "\n```"))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  (define text (string-append "Alias: $loinc = http://loinc.org\n\nProfile: BP\nParent: Observation\nId: bp\nTitle: \"BP\"\n"
                              "* component[systolic].code = $loinc#8480-6\n* valueQuantity 1..1\n\n"
                              "Profile: ChildBP\nParent: BP\n* status MS\n\nInstance: a\nInstanceOf: bp\n* status = #final\n"))
  (define es (entities-of text "/tmp/bp.fsh"))
  (check-equal? (map entity-name es) '("$loinc" "BP" "ChildBP" "a"))
  (check-equal? (entity-id (find-entity es "BP")) "bp")
  (check-equal? (rule-path-at text (cons 6 5)) "component[systolic].code")
  (check-false (rule-path-at text (cons 6 30)))
  (check-false (rule-path-at text (cons 3 2)))
  (check-equal? (parent-of text es (hash)) "Observation")
  (check-equal? (parent-of "Profile: X\nParent: http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient\n" es (hash)) "us-core-patient")
  (define h (element-hover "Observation" "component[systolic].code"))
  (check-regexp-match #rx"Observation.component.code\\*\\* 1..1" h)
  (check-regexp-match #rx"CodeableConcept" h)
  (check-regexp-match #rx"Observation.value\\[x\\]" (element-hover "Observation" "valueQuantity"))
  (check-false (element-hover "Observation" "nosuchelement"))
  (check-regexp-match #rx"Profile: BP\nParent: Observation" (entity-hover (find-entity es "BP") text))
  (check-equal? (aliases-of text) (hash "$loinc" "http://loinc.org"))
  (check-equal? (parent-of "Profile: X\nParent: $obs\n" es (hash "$obs" "http://hl7.org/fhir/StructureDefinition/Observation")) "Observation"))
