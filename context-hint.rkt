#lang racket/base
;;; context-hint.rkt — an InstanceOf or Parent not found that a folder BESIDE the project defines says
;;; so, and how to bring it in: `--context <dir>` for `raco fsh check`, requiring that module first for
;;; a `#lang fsh` module (which registers what it defines). Read as text: no compile of the sibling.

(require racket/contract racket/file racket/path
         (only-in fhir/diagnostic diagnostic diagnostic-lang diagnostic-phase diagnostic-severity diagnostic-message diagnostic-loc))

(provide
 (contract-out
  ;; with-context-hint : diagnostic dir (listof dir) [#:module?] -> diagnostic
  [with-context-hint (->* (any/c path-string? (listof path-string?)) (#:module? boolean?) any/c)]
  [defined-beside (-> path-string? (listof path-string?) string? (or/c pair? #f))]
  [shown-relative (-> path-string? string?)]))

;; with-context-hint : diagnostic dir context -> diagnostic   an InstanceOf or Parent not found that a
;; folder BESIDE the project defines says so, with the --context that brings it in (a new user checked
;; fsh-invalid/ alone, its profile next door in fsh/, and was told only "not found")
(define (with-context-hint d dir context #:module? [module? #f])
  (define m (regexp-match #px"(?:InstanceOf|Parent) `([^`]+)` not found" (diagnostic-message d)))
  (define where (and m (defined-beside dir context (cadr m))))
  (if where
      (diagnostic (diagnostic-lang d) (diagnostic-phase d) (diagnostic-severity d)
                  (if module?
                      ;; a module alone cannot see another file's profile; required after that file's
                      ;; module it can (fhir/registry), and `raco fsh check` takes the folder as context
                      (format "~a (`~a` is defined in ~a: require that module before this one, or check this folder with `raco fsh check ~a --context ~a`)"
                              (diagnostic-message d) (cadr m) (shown-relative (car where))
                              (regexp-replace #rx"(.)/$" (shown-relative dir) "\\1") (shown-relative (cdr where)))
                      (format "~a (`~a` is defined in ~a: check with `--context ~a`)" (diagnostic-message d) (cadr m)
                              (shown-relative (car where)) (shown-relative (cdr where))))
                  (diagnostic-loc d))
      d))

;; defined-beside : dir context name -> (cons file folder) | #f   a sibling folder's .fsh file that
;; declares a profile, extension, logical model or resource of this name (or Id), read as text
(define (defined-beside dir context name)
  ;; folders compared as directory paths: `.` is "/a/b/" and a listed sibling "/a/b", so the project's
  ;; own folder was taken for a sibling (a profile that failed to compile there was "defined in" its
  ;; own file, to be checked "with --context <this folder>")
  (define (folder p) (path->directory-path (simplify-path (path->complete-path p))))
  (define here (folder (if (file-exists? dir) (let-values ([(d _n _x) (split-path dir)]) d) dir)))
  (define parent (simplify-path (build-path here 'up)))
  (define taken (map folder context))
  (define rx (pregexp (string-append "(?m:^(?:Profile|Extension|Logical|Resource|Id):\\s*" (regexp-quote name) "\\s*$)")))
  (for*/first ([sib (in-list (with-handlers ([exn:fail? (lambda (_) '())]) (directory-list parent #:build? #t)))]
               #:when (directory-exists? sib)
               #:unless (equal? (folder sib) here)
               #:unless (member (folder sib) taken)
               [f (in-directory sib)]
               #:when (regexp-match? #rx"[.]fsh$" (path->string f))
               #:when (regexp-match? rx (file->string f)))
    (cons f sib)))

;; shown-relative : path -> string   relative to the current directory when that is shorter
(define (shown-relative p)
  (define full (simplify-path (path->complete-path p)))
  (define rel (path->string (find-relative-path (simplify-path (current-directory)) full)))
  (if (< (string-length rel) (string-length (path->string full))) rel (path->string full)))


(module+ test
  (require rackunit)
  ;; a name the project's OWN folder declares is not "beside" it, however the folder is written;
  ;; a sibling folder's is
  (define root (make-temporary-file "fsh-context-hint-~a" 'directory))
  (for ([d '("project" "next")]) (make-directory (build-path root d)))
  (with-output-to-file (build-path root "project" "a.fsh") (lambda () (display "Profile: Mine\nParent: Patient\n")))
  (with-output-to-file (build-path root "next" "b.fsh") (lambda () (display "Profile: Theirs\nParent: Patient\n")))
  (parameterize ([current-directory (build-path root "project")])
    (check-false (defined-beside "." '() "Mine"))
    (check-false (defined-beside (build-path root "project") '() "Mine"))
    (check-true (pair? (defined-beside "." '() "Theirs")))
    (check-false (defined-beside "." (list (build-path root "next")) "Theirs") "already in context"))
  (delete-directory/files root))
