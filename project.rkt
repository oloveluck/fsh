#lang racket/base

;;; project.rkt — an FSH project as a unit: find its files and sushi-config.yaml, parse each file
;;; (positions name the file), compile (dependency packages in the SD context), and pack.
;;; A project may also hold MAPS (`.map`/`.fml`, the FHIR Mapping Language): they are parsed with the
;;; fml package (loaded on demand — fsh does not depend on it) and their StructureMaps join the
;;; artifacts, so the package carries the structures and the arrows between them; `check` checks
;;; each map against the project's structures (check.rkt).
;;;
;;; compile-project : source -> project   never exits; everything wrong is in `diagnostics`
;;;   (fhir/diagnostic: parse errors per file; an entity that fails to compile, positioned at its
;;;   declaration). With parse errors nothing is compiled.
;;; pack-project! : project out.tgz -> path   the FHIR NPM package (pack.rkt)
;;; `raco fsh` (cli.rkt) is a thin shell over these.

(require racket/contract racket/list racket/file racket/path racket/match
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/diagnostic diagnostic loc loc-source loc-line diagnostic-loc)
         (only-in "compile.rkt" compile-doc/diagnostics entity-label emit-snapshot? canonical-base fsh-fhir-version instance-id)
         (only-in "context.rkt" fsh-config-status fsh-config-version)
         (only-in "yaml.rkt" yref yaml-boolean)
         (only-in "parser/parse.rkt" parse-fsh-doc/diagnostics)
         (only-in "parser/ast.rkt" fsh-instance? fsh-instance-metadata entity-name entity-metadata
                  fsh-profile? fsh-extension? fsh-logical? fsh-resource? fsh-valueset? fsh-codesystem?)
         "ig-config.rkt" "pack.rkt")

(provide
 (struct-out project)
 (contract-out
  [find-config     (-> path-string? (or/c path? #f))]
  [fsh-files       (-> path-string? (listof path?))]
  [map-files       (-> path-string? (listof path?))]
  [compile-project (->* (path-string?) (#:canonical (or/c string? #f) #:fhir-version (or/c string? #f)
                                        #:snapshot? boolean? #:texts (hash/c string? string?)
                                        #:packages (listof path-string?) #:context (listof path-string?)
                                        #:files (or/c (listof path-string?) #f))
                        project?)]
  [pack-project!   (-> project? path-string? path?)]
  ;; the file name of each of the project's artifacts, as the command line and the package write them
  [project-file-names (-> project? (listof string?))]))

;; project : config(ig-config|#f) fhir-version files texts(hash path-string -> text) located artifacts
;;           diagnostics missing-dependencies
;; sd-packages: the packages the compile resolved against (dependencies, then core), in order
;; maps: (listof (cons path-string fml-map)) — the project's maps, parsed (fml's AST)
(struct project (config fhir-version files texts located artifacts diagnostics missing sd-packages maps) #:transparent)

;; map-files : path -> (listof path)   every .map / .fml under a directory
(define (map-files src)
  (cond [(directory-exists? src) (source-files src #rx"[.](map|fml)$")]
        [else '()]))

;; source-files : dir regexp -> (listof path)   the files under a directory whose names match, by
;; name. Not a file or folder whose name starts with a dot: an editor's lock file beside the one
;; being edited (Emacs writes `.#bp.fsh`, a link to nothing, as soon as a buffer changes — the
;; language server then failed the whole project with "cannot open input file"), macOS's `._bp.fsh`,
;; a `.git` folder. And only what can be read: a link whose target is gone is no source.
(define (source-files src rx)
  (define (hidden? p) (regexp-match? #rx"^[.]" (path->string (let-values ([(_b n _d) (split-path p)]) n))))
  (sort (for/list ([f (in-directory src (lambda (d) (not (hidden? d))))]
                   #:when (regexp-match? rx (path->string f))
                   #:unless (hidden? f)
                   #:when (file-exists? f))
          f)
        string<? #:key path->string))

;; fml-entry : symbol -> procedure | #f   a function of the fml package, when it is installed
(define (fml-entry name)
  (with-handlers ([exn:fail? (lambda (_) #f)]) (dynamic-require 'fml/project name)))

;; parse-maps : files -> (values (listof (cons path-string map)) diagnostics)
;; (fml is loaded only when there is a map to parse: a project without maps paid ~0.25 s for it)
(define (parse-maps files)
  (define parse (and (pair? files) (fml-entry 'parse-map-file)))
  (cond
    [(null? files) (values '() '())]
    [(not parse) (values '() (list (diagnostic 'fsh 'compile 'error
                                               (format "~a map file~a found, but the fml package is not installed (raco pkg install fml)"
                                                       (length files) (if (= 1 (length files)) "" "s"))
                                               (loc (path->string (car files)) #f #f #f #f))))]
    [else
     (for/fold ([maps '()] [diags '()] #:result (values (reverse maps) diags)) ([f (in-list files)])
       (define-values (m ds) (parse f))
       (values (if m (cons (cons (path->string f) m) maps) maps) (append diags ds)))]))

;; find-config : path -> path|#f   sushi-config.yaml next to the sources or one/two levels up
(define (find-config src)
  (define dir (simplify-path (path->complete-path (if (directory-exists? src) src (or (path-only src) (current-directory))))))
  (for/or ([d (list dir (build-path dir 'up) (build-path dir 'up 'up))])
    (define f (simplify-path (build-path d "sushi-config.yaml")))
    (and (file-exists? f) f)))

;; fsh-files : path -> (listof path)   a file, or every .fsh under a directory, sorted
(define (fsh-files src)
  (cond [(directory-exists? src) (source-files src #rx"[.]fsh$")]
        [(file-exists? src) (list (if (path? src) src (string->path src)))]
        [else '()]))

;; #:texts — the text of a file as an editor holds it (by its complete path), in place of the disk
;; #:files — the project's files, in place of every .fsh and .map under `src` (a workspace knows
;; which files are its own: not its data, nor a text export of its store)
(define (compile-project src #:canonical [canonical* #f] #:fhir-version [fhir-version* #f] #:snapshot? [snapshot? #f]
                         #:texts [overrides (hash)] #:packages [extra-packages '()] #:context [context '()]
                         #:files [given #f])
  (define cfg (let ([f (find-config src)]) (and f (read-ig-config f))))
  (define canonical (or canonical* (and cfg (ig-config-canonical cfg))))
  (define fhir-version (or fhir-version* (and cfg (ig-config-fhir-version cfg)) "5.0.0"))
  (define-values (dep-packages missing) (dependency-packages (if cfg (ig-config-dependencies cfg) '())))
  ;; one tower per process: register the one this compile targets
  (define r4? (regexp-match? #rx"^4[.]" fhir-version))
  (define sd-packages
    (let ([core (spec-package-path (if r4? "hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz") #:optional? #t)])
      ;; after the declared dependencies, the packages SUSHI loads implicitly (extensions,
      ;; terminology) when a copy is at hand, then core
      ;; #:packages — packages the caller puts in context (a workspace's), after the declared ones
      (append dep-packages extra-packages (implicit-packages fhir-version) (if core (list core) '()))))
  (dynamic-require (if r4? 'fhir/r4/schemas 'fhir/r5/schemas) #f)
  (define (given-with rx) (filter (lambda (f) (regexp-match? rx (path->string f))) (map (lambda (f) (if (path? f) f (string->path f))) given)))
  (define files (if given (given-with #rx"[.]fsh$") (fsh-files src)))
  (define mfiles (if given (given-with #rx"[.](map|fml)$") (map-files src)))
  ;; #:context — other directories whose DEFINITIONS (profiles, extensions, value sets, rule sets,
  ;; aliases…) this project compiles against, without their instances and without reporting on them:
  ;; a folder of examples meant to fail, checked against the project's profiles in another folder
  (define own (for/list ([f (in-list files)]) (path->string (simplify-path (path->complete-path f)))))
  (define ctx-files (for*/list ([d (in-list context)] [f (in-list (fsh-files d))]
                                #:unless (member (path->string (simplify-path (path->complete-path f))) own))
                      f))
  ;; a leading #lang line is blanked, not removed, so line numbers stay true
  (define texts (for/hash ([f (in-list (append files ctx-files mfiles))])
                  (define text (or (hash-ref overrides (path->string (simplify-path (path->complete-path f))) #f) (file->string f)))
                  (values (path->string f) (regexp-replace #px"^#lang[^\n]*" text ""))))
  (define-values (maps map-diags) (parse-maps mfiles))
  (define structuremaps (let ([to-sm (and (pair? maps) (fml-entry 'map-structuremap))]) (if to-sm (map (lambda (p) (to-sm (cdr p))) maps) '())))
  ;; every file's entities that parse, with every parse error and import-level check (fsh/lint)
  (define-values (located parse-errors)
    (for/fold ([located '()] [errors '()] #:result (values located errors))
              ([f (in-list (append files ctx-files))])
      (define ctx? (memq f ctx-files))
      (define-values (es ds) (parse-fsh-doc/diagnostics (hash-ref texts (path->string f)) #:source (path->string f)))
      (values (append located (if ctx? (filter (lambda (l) (not (fsh-instance? (car l)))) es) es))
              (append errors (if ctx? '() ds)))))
  (cond
    [(pair? parse-errors) (project cfg fhir-version files texts located '() (append parse-errors map-diags) missing sd-packages maps)]
    [else
     (define-values (out errors rule-diags)
       (parameterize ([emit-snapshot? snapshot?]
                      [canonical-base (or canonical (canonical-base))]
                      [fsh-fhir-version fhir-version]
                      [fsh-config-status (and cfg (ig-config-status cfg))]
                      [fsh-config-version (and cfg (eq? (yaml-boolean (yref (ig-config-yaml cfg) "FSHOnly")) #t)
                                               (ig-config-version cfg))]
                      [current-sd-packages sd-packages])
         (compile-doc/diagnostics (map car located))))
     (define at (for/hash ([p (in-list located)]) (values (entity-label (car p)) (cdr p))))
     ;; rule-level diagnostics (an unknown element, …) first, then entities that failed outright
     (define (own-diagnostic? d) (let ([l (diagnostic-loc d)]) (not (and l (loc-source l) (for/or ([f (in-list ctx-files)]) (equal? (loc-source l) (path->string f)))))))
     (project cfg fhir-version files texts located (append out structuremaps)
              (append (filter own-diagnostic? rule-diags)
                      (filter own-diagnostic? (duplicate-id-diagnostics located))
                      (for/list ([e (in-list errors)])
                        (diagnostic 'fsh 'compile 'error (format "~a: ~a" (car e) (exn-message (cdr e))) (hash-ref at (car e) #f)))
                      map-diags)
              missing sd-packages maps)]))

;; duplicate-id-diagnostics : located -> diagnostics   two definitions of one resource type with one id
;; (`Id: x`, or the name when there is none) would be one artifact, the later silently replacing the
;; earlier: an error at the later one, naming where the first is
(define (duplicate-id-diagnostics located)
  ;; an Instance's kind is what it is an instance OF (two Observations, or two instances of one
  ;; profile, with one id overwrite each other in a package or a store); inline ones are embedded
  (define (instance-kind e)
    (define m (fsh-instance-metadata e))
    (define io (assoc 'instanceOf m))
    (define u (assoc 'usage m))
    (and io (not (and u (equal? (cdr u) "#inline"))) (format "~a instance" (cdr io))))
  (define (kind e)
    (cond [(or (fsh-profile? e) (fsh-extension? e) (fsh-logical? e) (fsh-resource? e)) "StructureDefinition"]
          [(fsh-valueset? e) "ValueSet"] [(fsh-codesystem? e) "CodeSystem"]
          [(fsh-instance? e) (instance-kind e)] [else #f]))
  (define (id-of e)
    (if (fsh-instance? e)
        (instance-id e)
        (let ([i (assoc 'id (entity-metadata e))]) (if i (cdr i) (entity-name e)))))
  (let loop ([ls located] [seen (hash)] [out '()])
    (match ls
      ['() (reverse out)]
      [(cons (cons e l) rest)
       (define k (kind e))
       (define key (and k (cons k (id-of e))))
       (cond [(and key (hash-ref seen key #f))
              => (lambda (first-l)
                   (loop rest seen
                         (cons (diagnostic 'fsh 'compile 'error
                                           (format "~a: the ~a id `~a` is already used~a" (entity-label e) k (cdr key)
                                                   (if (and first-l (loc-source first-l) (loc-line first-l))
                                                       (format " (~a:~a)" (loc-source first-l) (loc-line first-l)) ""))
                                           l)
                               out)))]
             [key (loop rest (hash-set seen key l) out)]
             [else (loop rest seen out)])])))

;; pack-project! : project out -> path   instances' Usage decides package/ vs package/example/
(define (pack-project! p out)
  (unless (project-config p) (raise-user-error 'fsh "packing needs the project's sushi-config.yaml (package id, version, dependencies)"))
  (define usages (for*/hash ([l (in-list (project-located p))] #:when (fsh-instance? (car l))
                             [m (in-value (fsh-instance-metadata (car l)))]
                             [u (in-value (assoc 'usage m))] #:when u)
                   (values (instance-id (car l)) (cdr u))))
  (pack-package! out (project-config p) (project-artifacts p) usages #:unnamed (unnamed-instances p)))

;; unnamed-instances : project -> (listof string)   the Instances whose resource carries no id, in
;; project order: those that are not inline and whose id no artifact has (an instance of a logical
;; model — the model has no `id` element, so the name is all that tells two of them apart)
(define (unnamed-instances p)
  (define ids (for/hash ([a (in-list (project-artifacts p))] #:when (hash-ref a 'id #f)) (values (hash-ref a 'id) #t)))
  (for/list ([l (in-list (project-located p))]
             #:when (fsh-instance? (car l))
             #:unless (equal? (assoc-ref* (fsh-instance-metadata (car l)) 'usage) "#inline")
             #:unless (hash-ref ids (instance-id (car l)) #f))
    (instance-id (car l))))
(define (assoc-ref* m k) (let ([p (assoc k m)]) (and p (cdr p))))
;; project-file-names : project -> (listof string)
(define (project-file-names p) (artifact-file-names (project-artifacts p) (unnamed-instances p)))

(module+ test
  (require rackunit racket/file fhir/diagnostic)
  ;; rule-level errors accumulate: every unknown element is reported, each at its own rule, and
  ;; the entity still compiles
  (define dir (make-temporary-directory))
  (call-with-output-file (build-path dir "bad.fsh")
    (λ (o) (void (write-string (string-append "Profile: Bad\nParent: Patient\n* nosuchelement 1..1\n"
                                        "* name 1..1 MS\n* gender and birthDat MS\n\n"
                                        "Instance: P1\nInstanceOf: Patient\n* nmae.given = \"x\"\n") o))))
  (define p (compile-project dir))
  (define ds (project-diagnostics p))
  (check-equal? (map (λ (d) (loc-line (diagnostic-loc d))) ds) '(3 5 9))
  (check-equal? (map (λ (d) (loc-column (diagnostic-loc d))) ds) '(3 3 3))
  (check-true (for/and ([d (in-list ds)]) (regexp-match? #rx"is not an element of Patient" (diagnostic-message d))))
  (check-equal? (length (project-artifacts p)) 2)
  ;; an editor's unsaved text stands in for the file on disk
  (define p* (compile-project dir #:texts (hash (path->string (simplify-path (path->complete-path (build-path dir "bad.fsh"))))
                                                "Profile: Good\nParent: Patient\n* name 1..1 MS\n")))
  (check-equal? (project-diagnostics p*) '())
  (check-equal? (length (project-artifacts p*)) 1)
  (delete-directory/files dir)
  ;; an instance whose id comes from an `* id = "…"` rule keeps its Usage when packed: compile,
  ;; check and pack share one instance-id (pack used to key Usage by `Id:` or the name only, so this
  ;; #definition Basic went to package/example/; a conformance resource goes to package/ anyway)
  (define d3 (make-temporary-directory))
  (call-with-output-file (build-path d3 "sushi-config.yaml")
    (λ (o) (void (write-string "id: example.ids\ncanonical: http://example.org\nname: Ids\nstatus: draft\nversion: 0.1.0\nfhirVersion: 4.0.1\nFSHOnly: true\n" o))))
  (call-with-output-file (build-path d3 "basic.fsh")
    (λ (o) (void (write-string "Instance: SomeBasic\nInstanceOf: Basic\nUsage: #definition\n* id = \"from-rule\"\n* code.text = \"x\"\n" o))))
  (define out (build-path d3 "out.tgz"))
  (void (pack-project! (compile-project d3) out))
  (define-values (_m defs examples) ((dynamic-require 'fhir/package 'read-package) out))
  (check-equal? (map car defs) '("Basic-from-rule.json"))
  (check-equal? examples '())
  (delete-directory/files d3)
  ;; a project with a map (when the fml package is installed): the StructureMap joins the artifacts,
  ;; a parse error in the map is a diagnostic positioned in it
  (when (fml-entry 'parse-map-file)
    (define d2 (make-temporary-directory))
    (call-with-output-file (build-path d2 "model.fsh")
      (λ (o) (void (write-string "Logical: TLeft\nId: TLeft\n* a 0..1 string \"a\"\n" o))))
    (call-with-output-file (build-path d2 "copy.map")
      (λ (o) (void (write-string (string-append "map \"http://example.org/StructureMap/copy\" = \"copy\"\n"
                                          "uses \"http://example.org/StructureDefinition/TLeft\" alias TLeft as source\n"
                                          "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\n"
                                          "group g(source src : TLeft, target tgt : Patient) { src.a as a -> tgt.id = a; }\n") o))))
    (define p2 (compile-project d2 #:canonical "http://example.org"))
    (check-equal? (project-diagnostics p2) '())
    (check-equal? (sort (map (λ (a) (hash-ref a 'resourceType)) (project-artifacts p2)) string<?) '("StructureDefinition" "StructureMap"))
    (check-equal? (length (project-maps p2)) 1)
    (call-with-output-file (build-path d2 "bad.map") (λ (o) (void (write-string "map oops\n" o))))
    (define p3 (compile-project d2 #:canonical "http://example.org"))
    (check-equal? (length (project-diagnostics p3)) 1)
    (check-true (regexp-match? #rx"bad[.]map" (loc-source (diagnostic-loc (car (project-diagnostics p3))))))
    (delete-directory/files d2)))

(module+ test
  ;; an instance of a logical model has no id: its file is named by the Instance, so two of them
  ;; are two files (they were `<type>-instance.json`, and one file in a package)
  (define lm (make-temporary-directory))
  (call-with-output-file (build-path lm "a.fsh")
    (λ (o) (void (write-string (string-append "Logical: Intake\nId: intake\nTitle: \"Intake\"\nDescription: \"x\"\n* name 1..1 string \"name\"\n\n"
                                              "Instance: ann-intake\nInstanceOf: Intake\n* name = \"Ann\"\n\n"
                                              "Instance: bob-intake\nInstanceOf: Intake\n* name = \"Bob\"\n") o))))
  (check-equal? (project-file-names (compile-project lm #:fhir-version "4.0.1"))
                '("StructureDefinition-intake.json" "intake-ann-intake.json" "intake-bob-intake.json"))
  (delete-directory/files lm))

(module+ test
  ;; an editor's lock file is no source: Emacs puts `.#bp.fsh` (a link to nothing) beside a file
  ;; being edited, and the project failed with "cannot open input file"
  (define ed (make-temporary-directory))
  (call-with-output-file (build-path ed "bp.fsh") (λ (o) (void (write-string "Profile: BP\nParent: Observation\n* subject 1..1\n" o))))
  (make-file-or-directory-link "someone@host.123:456" (build-path ed ".#bp.fsh"))
  (call-with-output-file (build-path ed "._bp.fsh") (λ (o) (void (write-string "\0\5\26\7" o))))
  (make-directory (build-path ed ".cache"))
  (call-with-output-file (build-path ed ".cache" "old.fsh") (λ (o) (void (write-string "Profile: Old\nParent: Nope\n" o))))
  (call-with-output-file (build-path ed ".#m.map") (λ (o) (void (write-string "x" o))))
  (check-equal? (map (λ (f) (path->string (file-name-from-path f))) (fsh-files ed)) '("bp.fsh"))
  (check-equal? (map-files ed) '())
  (check-equal? (project-diagnostics (compile-project ed #:fhir-version "4.0.1")) '())
  (delete-directory/files ed))
