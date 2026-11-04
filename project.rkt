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

(require racket/contract racket/list racket/file racket/path
         (only-in fhir/sd/sd-index current-sd-packages)
         (only-in fhir/spec-package spec-package-path)
         (only-in fhir/diagnostic diagnostic loc exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in "compile.rkt" compile-doc/diagnostics entity-label emit-snapshot? canonical-base fsh-fhir-version)
         (only-in "parser/parse.rkt" parse-fsh-doc/located)
         (only-in "parser/ast.rkt" fsh-instance? fsh-instance-metadata fsh-instance-name)
         "ig-config.rkt" "pack.rkt")

(provide
 (struct-out project)
 (contract-out
  [find-config     (-> path-string? (or/c path? #f))]
  [fsh-files       (-> path-string? (listof path?))]
  [map-files       (-> path-string? (listof path?))]
  [compile-project (->* (path-string?) (#:canonical (or/c string? #f) #:fhir-version (or/c string? #f)
                                        #:snapshot? boolean?)
                        project?)]
  [pack-project!   (-> project? path-string? path?)]))

;; project : config(ig-config|#f) fhir-version files texts(hash path-string -> text) located artifacts
;;           diagnostics missing-dependencies
;; sd-packages: the packages the compile resolved against (dependencies, then core), in order
;; maps: (listof (cons path-string fml-map)) — the project's maps, parsed (fml's AST)
(struct project (config fhir-version files texts located artifacts diagnostics missing sd-packages maps) #:transparent)

;; map-files : path -> (listof path)   every .map / .fml under a directory
(define (map-files src)
  (cond [(directory-exists? src)
         (sort (for/list ([f (in-directory src)] #:when (regexp-match? #rx"[.](map|fml)$" (path->string f))) f)
               string<? #:key path->string)]
        [else '()]))

;; fml-entry : symbol -> procedure | #f   a function of the fml package, when it is installed
(define (fml-entry name)
  (with-handlers ([exn:fail? (lambda (_) #f)]) (dynamic-require 'fml/project name)))

;; parse-maps : files -> (values (listof (cons path-string map)) diagnostics)
(define (parse-maps files)
  (define parse (fml-entry 'parse-map-file))
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
  (cond [(directory-exists? src)
         (sort (for/list ([f (in-directory src)] #:when (regexp-match? #rx"[.]fsh$" (path->string f))) f)
               string<? #:key path->string)]
        [(file-exists? src) (list (if (path? src) src (string->path src)))]
        [else '()]))

(define (compile-project src #:canonical [canonical* #f] #:fhir-version [fhir-version* #f] #:snapshot? [snapshot? #f])
  (define cfg (let ([f (find-config src)]) (and f (read-ig-config f))))
  (define canonical (or canonical* (and cfg (ig-config-canonical cfg))))
  (define fhir-version (or fhir-version* (and cfg (ig-config-fhir-version cfg)) "5.0.0"))
  (define-values (dep-packages missing) (dependency-packages (if cfg (ig-config-dependencies cfg) '())))
  ;; one tower per process: register the one this compile targets
  (define r4? (regexp-match? #rx"^4[.]" fhir-version))
  (define sd-packages
    (let ([core (spec-package-path (if r4? "hl7.fhir.r4.core.tgz" "hl7.fhir.r5.core.tgz") #:optional? #t)])
      (append dep-packages (if core (list core) '()))))
  (dynamic-require (if r4? 'fhir/r4/schemas 'fhir/r5/schemas) #f)
  (define files (fsh-files src))
  (define mfiles (map-files src))
  ;; a leading #lang line is blanked, not removed, so line numbers stay true
  (define texts (for/hash ([f (in-list (append files mfiles))])
                  (values (path->string f) (regexp-replace #px"^#lang[^\n]*" (file->string f) ""))))
  (define-values (maps map-diags) (parse-maps mfiles))
  (define structuremaps (let ([to-sm (fml-entry 'map-structuremap)]) (if to-sm (map (lambda (p) (to-sm (cdr p))) maps) '())))
  (define parse-errors '())
  (define located
    (append*
     (for/list ([f (in-list files)])
       (with-handlers ([exn:fail:diagnostic? (lambda (e) (set! parse-errors (cons (exn:fail:diagnostic-diagnostic e) parse-errors)) '())])
         (parse-fsh-doc/located (hash-ref texts (path->string f)) #:source (path->string f))))))
  (cond
    [(pair? parse-errors) (project cfg fhir-version files texts located '() (append (reverse parse-errors) map-diags) missing sd-packages maps)]
    [else
     (define-values (out errors rule-diags)
       (parameterize ([emit-snapshot? snapshot?]
                      [canonical-base (or canonical (canonical-base))]
                      [fsh-fhir-version fhir-version]
                      [current-sd-packages sd-packages])
         (compile-doc/diagnostics (map car located))))
     (define at (for/hash ([p (in-list located)]) (values (entity-label (car p)) (cdr p))))
     ;; rule-level diagnostics (an unknown element, …) first, then entities that failed outright
     (project cfg fhir-version files texts located (append out structuremaps)
              (append rule-diags
                      (for/list ([e (in-list errors)])
                        (diagnostic 'fsh 'compile 'error (format "~a: ~a" (car e) (exn-message (cdr e))) (hash-ref at (car e) #f)))
                      map-diags)
              missing sd-packages maps)]))

;; pack-project! : project out -> path   instances' Usage decides package/ vs package/example/
(define (pack-project! p out)
  (unless (project-config p) (raise-user-error 'fsh "packing needs the project's sushi-config.yaml (package id, version, dependencies)"))
  (define usages (for*/hash ([l (in-list (project-located p))] #:when (fsh-instance? (car l))
                             [m (in-value (fsh-instance-metadata (car l)))]
                             [u (in-value (assoc 'usage m))] #:when u)
                   (values (let ([i (assoc 'id m)]) (if i (cdr i) (fsh-instance-name (car l)))) (cdr u))))
  (pack-package! out (project-config p) (project-artifacts p) usages))

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
  (delete-directory/files dir)
  ;; a project with a map (when the fml package is installed): the StructureMap joins the artifacts,
  ;; a parse error in the map is a diagnostic positioned in it
  (when (fml-entry 'parse-map-file)
    (define d2 (make-temporary-directory))
    (call-with-output-file (build-path d2 "model.fsh")
      (λ (o) (void (write-string "Logical: TLeft
Id: TLeft
* a 0..1 string "a"
" o))))
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
