#lang racket/base

;;; check.rkt — `raco fsh check`: an FSH project end to end, saying WHERE IN THE FSH each problem is.
;;;
;;; The loop a spec author (or an agent) iterates against: compile the project (fsh/project: parse
;;; and rule-level diagnostics), validate every example it defines against the profiles it defines
;;; (conformance/validate, in the package context the compiler used: the project's own artifacts,
;;; its dependencies, core), and — when a CapabilityStatement is named — generate the server from the
;;; packed project and report the requirements it does not meet (fhir-server's store/ig-server gaps,
;;; loaded on demand: fsh depends on fhir-validation, not on the server).
;;;
;;; A validation issue speaks FHIR (`Observation.component.code minimum required = 1`); here it is
;;; traced to the FSH that produced it: positioned at the instance rule that wrote that element (else
;;; the instance's header), and naming the profile rule that constrains it when one does.
;;;
;;; check-fsh-project : dir [#:capability id] -> check-report
;;;   never exits; everything is a fhir/diagnostic (lang 'fsh, phase 'compile | 'validate | 'server)

(require racket/contract racket/list racket/string racket/file
         (only-in fhir/diagnostic diagnostic diagnostic-severity diagnostic-loc loc loc-source loc-line loc-column
                  attached-loc)
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/conformance/validate validate-issues issue-severity issue-path issue-msg)
         (only-in fhir/serialize/read-json strip-jnum)
         (only-in "pack.rkt" pack-package!)
         (only-in "ig-config.rkt" ig-config)
         (only-in "project.rkt" compile-project pack-project! project-artifacts project-diagnostics project-located
                  project-sd-packages project-config project-texts project-fhir-version project-maps)
         (only-in "compile.rkt" rule-path-of)
         (only-in "parser/ast.rkt" fsh-instance? fsh-instance-name fsh-instance-metadata fsh-instance-rules
                  fsh-profile? fsh-profile-name fsh-profile-metadata fsh-profile-rules
                  rule-assign? rule-assign-path rule-path? rule-path-path))

(provide
 (struct-out check-report)
 (contract-out
  [check-fsh-project (->* (path-string?) (#:capability (or/c string? #f) #:canonical (or/c string? #f)
                                          #:fhir-version (or/c string? #f) #:packages (listof path-string?))
                          check-report?)]
  [trace-rule        (-> list? string? (or/c any/c #f))]))

;; A check-report: every diagnostic (compile first, then validation, then server), the source texts
;; (for excerpts), the counts, and the compiled artifacts.
(struct check-report (diagnostics texts examples invalid artifacts) #:transparent)

(define (check-fsh-project dir #:capability [capability #f] #:canonical [canonical #f] #:fhir-version [fhir-version #f]
                           #:packages [packages '()])
  (define p (compile-project dir #:canonical canonical #:fhir-version fhir-version #:packages packages))
  (define compile-diags (project-diagnostics p))
  (define-values (examples invalid validation-diags) (validate-examples p))
  (define map-diags (check-maps p examples))
  (define server-diags (if capability (server-gaps p capability) '()))
  (check-report (append compile-diags validation-diags map-diags server-diags)
                (project-texts p)
                (length examples)
                invalid
                (project-artifacts p)))

;; ---- the project's maps, against its structures ----------------------------------------------------
;; check-maps : project examples -> diagnostics   each map (fml/project, loaded on demand) checked in
;; the package context the compile used, the project's own artifacts first: element paths against the
;; structures, then the examples of its source structure transformed and validated against its target
(define (check-maps p examples)
  (define maps (project-maps p))
  (define check (and (pair? maps) (with-handlers ([exn:fail? (lambda (_) #f)]) (dynamic-require 'fml/project 'check-project-map))))
  (cond
    [(not check) '()]
    [else
     (define artifacts (project-artifacts p))
     (define profiles (filter (lambda (a) (equal? (hash-ref a 'resourceType #f) "StructureDefinition")) artifacts))
     (define tmp (make-temporary-directory "fsh-check-~a"))
     (define own (artifacts-package! tmp (project-config p) artifacts))
     (define ds
       (parameterize ([current-sd-packages (cons own (project-sd-packages p))])
         (append* (for/list ([m (in-list maps)])
                    (check (cdr m) (map strip-jnum examples) profiles #:source (car m))))))
     (delete-directory/files tmp #:must-exist? #f)
     ds]))

;; ---- validation, traced to the FSH ----------------------------------------------------------------

;; validate-examples : project -> (values examples invalid-count diagnostics)
;; the instances the package would carry as examples (Usage #example, the default), each validated
;; against its base type and the profiles it claims, the project's own artifacts first in context
(define (validate-examples p)
  (define located (project-located p))
  (define artifacts (project-artifacts p))
  (define instances                                  ; id -> (entity . loc)
    (for/hash ([l (in-list located)] #:when (fsh-instance? (car l)))
      (values (instance-id (car l)) l)))
  (define profiles                                   ; canonical url and id -> (entity . loc)
    (for*/hash ([l (in-list located)] #:when (fsh-profile? (car l))
                [a (in-value (findf (lambda (a) (and (equal? (hash-ref a 'resourceType #f) "StructureDefinition")
                                                     (equal? (hash-ref a 'id #f) (profile-id (car l)))))
                                    artifacts))]
                #:when a
                [k (in-list (list (hash-ref a 'url "") (profile-id (car l))))])
      (values k l)))
  ;; the FSH instance an artifact came from: by id; an instance of a logical model carries no id,
  ;; so it is matched by the model it is an InstanceOf (its resourceType)
  (define (instance-for a)
    (or (let ([id (hash-ref a 'id #f)]) (and id (hash-ref instances id #f)))
        (for/first ([l (in-list located)]
                    #:when (and (fsh-instance? (car l))
                                (let ([io (assoc 'instanceOf (fsh-instance-metadata (car l)))])
                                  (and io (equal? (cdr io) (hash-ref a 'resourceType #f))))))
          l)))
  (define examples
    (for/list ([a (in-list artifacts)]
               #:when (let ([l (instance-for a)])
                        (and l (equal? (hash-ref a 'resourceType #f) (instance-type a))
                             (not (equal? (usage-of (car l)) "#definition")))))
      a))
  (define tmp (make-temporary-directory "fsh-check-~a"))
  (define own (artifacts-package! tmp (project-config p) artifacts))
  (define per-example
    (parameterize ([current-sd-packages (cons own (project-sd-packages p))])
      (for/list ([a (in-list examples)])
        (for/list ([i (in-list (validate-issues (strip-jnum a)))]
                   #:when (member (issue-severity i) '("error" "fatal" "warning")))
          (issue->diagnostic i a (instance-for a) profiles)))))
  (delete-directory/files tmp #:must-exist? #f)
  (values examples
          (for/sum ([ds (in-list per-example)]) (if (ormap (lambda (d) (eq? (diagnostic-severity d) 'error)) ds) 1 0))
          (append* per-example)))
(define (instance-type a) (hash-ref a 'resourceType #f))

;; issue->diagnostic : issue resource (entity . loc) profiles -> diagnostic
(define (issue->diagnostic i res inst profiles)
  (define path (issue-path i))
  (define rule (trace-rule (fsh-instance-rules (car inst)) path))
  (define at (or (and rule (attached-loc rule)) (cdr inst)))
  (define prof-note
    (for*/or ([url (in-list (let ([m (hash-ref res 'meta #f)]) (if (hash? m) (hash-ref m 'profile '()) '())))]
                 [pl (in-value (hash-ref profiles url #f))] #:when pl)
      (define pr (trace-rule (fsh-profile-rules (car pl)) path #:exact? #t))
      (define pl-loc (and pr (attached-loc pr)))
      (and pl-loc (format " (constrained by profile ~a at ~a)" (fsh-profile-name (car pl)) (loc-text pl-loc)))))
  (diagnostic 'fsh 'validate (string->symbol (if (equal? (issue-severity i) "warning") "warning" "error"))
              (format "instance ~a: ~a: ~a~a" (fsh-instance-name (car inst)) path (issue-msg i) (or prof-note ""))
              at))
(define (loc-text l) (if l (format "~a:~a:~a" (loc-source l) (loc-line l) (loc-column l)) "?"))

;; trace-rule : rules issue-path [#:exact? b] -> rule|#f
;; the rule whose FSH path best matches a validator path. `Observation.component.code` drops its
;; type root to `component.code`; a rule path drops brackets (`component[tumorSize].valueQuantity` ->
;; `component.valueQuantity`). The rule sharing the longest leading run of steps wins (the first
;; such rule). A path rooted at a datatype (`Timing.repeat.periodUnit`, the validator inside a nested
;; type) matches anywhere in a rule path. #:exact? (for profiles) wants the rule to name the element
;; itself or its parent, not a sibling.
(define (trace-rule rules issue-path #:exact? [exact? #f])
  (define segs (string-split issue-path "."))
  (define rooted-at-datatype? (and (pair? segs) (regexp-match? #px"^[A-Z]" (car segs))
                                   (not (resource-like? (car segs)))))
  (define want (map normalise-step (if (pair? segs) (cdr segs) '())))
  (define scored
    (for*/list ([r (in-list rules)]
                [rp (in-value (rule-path-string r))] #:when rp
                [steps (in-value (map normalise-step (string-split (path-without-brackets rp) ".")))]
                [s (in-value (if rooted-at-datatype? (infix-score want steps) (prefix-score want steps)))]
                #:when (and (> s 0) (or (not exact?) (>= s (max 1 (sub1 (length want)))))))
      (cons s r)))
  (and (pair? scored)
       (let ([best (apply max (map car scored))])
         (cdr (findf (lambda (p) (= (car p) best)) scored)))))

;; a validator path is rooted at the resource type for the resource's own elements; at a datatype
;; (`Timing`, `Quantity`) when it recurses into one
(define resource-like-cache (make-hash))
(define (resource-like? t)
  (hash-ref! resource-like-cache t
             (lambda () (not (member t '("Timing" "Quantity" "CodeableConcept" "Coding" "Identifier" "Reference"
                                         "Period" "Range" "Ratio" "HumanName" "Address" "ContactPoint" "Annotation"
                                         "Attachment" "Dosage" "Meta" "Narrative" "Extension" "SampledData"
                                         "Money" "Duration" "Age" "Count" "Distance" "Signature"))))))

(define (rule-path-string r)
  (cond [(rule-assign? r) (rule-assign-path r)]
        [(rule-path? r) (rule-path-path r)]
        [else (rule-path-of r)]))
(define (path-without-brackets p) (regexp-replace* #px"\\[[^\\]]*\\]" p ""))
;; `valueQuantity` and `value[x]` both name the choice element `value`
(define (normalise-step s)
  (cond [(regexp-match #px"^(value|effective|onset|abatement|deceased|multipleBirth|occurrence|performed|serviced|timing|asNeeded|medication|item|answer|product|rate|dose)[A-Z]" s)
         => cadr]
        [else (regexp-replace #px"\\[x\\]$" s "")]))
(define (prefix-score want steps)
  (let loop ([w want] [s steps] [n 0])
    (if (and (pair? w) (pair? s) (equal? (car w) (car s))) (loop (cdr w) (cdr s) (add1 n)) n)))
;; the issue's steps as a contiguous run inside the rule path
(define (infix-score want steps)
  (for/fold ([best 0]) ([i (in-range (length steps))])
    (max best (prefix-score want (drop steps i)))))

(define (instance-id e)
  (let ([m (assoc 'id (fsh-instance-metadata e))]) (if m (cdr m) (fsh-instance-name e))))
(define (profile-id e)
  (let ([m (assoc 'id (fsh-profile-metadata e))]) (if m (cdr m) (fsh-profile-name e))))
(define (usage-of e) (let ([u (assoc 'usage (fsh-instance-metadata e))]) (and u (cdr u))))

;; artifacts-package! : dir config|#f artifacts -> path   the project's artifacts as a package the SD
;; index reads (its own profiles and extensions resolve before any dependency's)
(define (artifacts-package! dir cfg artifacts)
  (define out (build-path dir "project.tgz"))
  (pack-package! out (or cfg (ig-config #f #f '() #f #f #f #f #f #f #f #f #f)) artifacts (hash))
  out)

;; ---- the server generated from the packed project -------------------------------------------------
;; server-gaps : project capability -> diagnostics   one warning per unmet requirement of that
;; CapabilityStatement (SHALL as error)
(define (server-gaps p capability)
  (with-handlers ([exn:fail:filesystem:missing-module?
                   (lambda (_) (list (diagnostic 'fsh 'server 'error
                                                 "--capability needs the server: raco pkg install fhir-server" #f)))])
    (if (project-config p)
        (server-gaps* p capability)
        ;; the server is generated from the project's package, which sushi-config.yaml describes
        (list (diagnostic 'fsh 'server 'error
                          "--capability needs the project's sushi-config.yaml: the server is generated from the project packed as a package (id, version, dependencies)" #f)))))
(define (server-gaps* p capability)
  (define make-ig-server (dynamic-require 'fhir/store/ig-server 'make-ig-server))
  (define gap->list (dynamic-require 'fhir/store/ig-server 'gap->list))
  (define ig-server-gaps (dynamic-require 'fhir/store/ig-server 'ig-server-gaps))
  (define open-fhir-store (dynamic-require 'fhir/store/fhir-sqlite 'open-fhir-store))
  (define close-fhir-store (dynamic-require 'fhir/store/fhir-sqlite 'close-fhir-store))
  (define dir (make-temporary-directory "fsh-check-~a"))
  (define tgz (build-path dir "package.tgz"))
  (pack-project! p tgz)
  (define store (open-fhir-store (build-path dir "store.db")))
  (define cs-loc
    (for/first ([l (in-list (project-located p))]
                #:when (and (fsh-instance? (car l)) (member capability (list (instance-id (car l)) (fsh-instance-name (car l))))))
      (cdr l)))
  (define diags
    (for/list ([g (in-list (ig-server-gaps (make-ig-server store tgz #:capability capability)))])
      (define parts (gap->list g))           ; type what name expectation reason
      (diagnostic 'fsh 'server (if (equal? (list-ref parts 3) "SHALL") 'error 'warning)
                  (format "~a: ~a ~a ~a not met: ~a" capability (list-ref parts 3) (list-ref parts 0)
                          (list-ref parts 2) (list-ref parts 4))
                  cs-loc)))
  (close-fhir-store store)
  (delete-directory/files dir #:must-exist? #f)
  diags)

(module+ test
  (require rackunit fsh/parser/ast)
  ;; tracing a validator path to the rule that wrote it
  (define rules (list (rule-assign "status" 'v #f)
                      (rule-assign "component[tumorSize].valueQuantity" 'v #f)
                      (rule-assign "component[tumorSize].code" 'v #f)
                      (rule-assign "dosageInstruction.timing.repeat.periodUnit" 'v #f)))
  (check-equal? (rule-assign-path (trace-rule rules "Observation.component.valueQuantity")) "component[tumorSize].valueQuantity")
  (check-equal? (rule-assign-path (trace-rule rules "Observation.component.value[x]")) "component[tumorSize].valueQuantity")
  (check-equal? (rule-assign-path (trace-rule rules "Observation.component.code")) "component[tumorSize].code")
  (check-equal? (rule-assign-path (trace-rule rules "Timing.repeat.periodUnit")) "dosageInstruction.timing.repeat.periodUnit")
  (check-false (trace-rule rules "Observation.subject"))
  ;; profiles: the element itself (or its parent), not a sibling
  (check-false (trace-rule (list (rule-assign "component.code" 'v #f)) "Observation.component.interpretation.text" #:exact? #t)))
