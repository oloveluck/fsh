#lang racket/base

;;; cli-test.rkt — `raco fsh` as a person calls it, on what it once got wrong:
;;;   * no argument at all was `car` of the empty list (a stack trace): it is the usage, exit 2;
;;;   * a Reference target pinned to a version (`Reference(Patient|4.0.1)`, as HRex pins its US Core
;;;     targets) was refused as a `|` list of targets, so such a project did not compile at all;
;;;     the spaced form (`Reference(Patient | Group)`), which FSH dropped, is still the error;
;;;   * a Mapping's `Target:` written without quotes compiled (FSH's grammar takes a string there).

(require rackunit racket/file racket/port racket/string racket/system)

;; fsh : string ... -> (values exit-code stdout stderr)   `raco fsh <args>` from directory `dir`
(define dir (make-temporary-directory "fsh-cli-~a"))
(define (fsh . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code (parameterize ([current-output-port out] [current-error-port err] [current-directory dir])
                 (apply system*/exit-code (find-executable-path (find-system-path 'exec-file)) "-l" "raco" "fsh" args)))
  (values code (get-output-string out) (get-output-string err)))
;; project! : string string -> void   a folder `name` under `dir` holding one FSH file of that text
(define (project! name text)
  (make-directory* (build-path dir name))
  (display-to-file text (build-path dir name "a.fsh") #:exists 'replace))

(test-case "no argument is the usage, exit 2"
  (define-values (code out err) (fsh))
  (check-equal? code 2)
  (check-equal? out "")
  (check-regexp-match #rx"^usage: raco fsh " err)
  (check-false (regexp-match? #rx"context[.][.][.]|contract violation" err) err))

(test-case "--help is the usage on stdout, exit 0"
  (define-values (code out err) (fsh "--help"))
  (check-equal? code 0)
  (check-regexp-match #rx"^usage: raco fsh " out))

(test-case "a Reference target pinned to a version compiles"
  (project! "pinned" (string-append "Profile: PinnedObservation\nParent: Observation\n"
                                    "* subject only Reference(Patient|4.0.1 or Group)\n"))
  (define-values (code out err) (fsh "pinned" "--fhir-version" "4.0.1" "-o" "pinned-out"))
  (check-equal? code 0 err)
  (check-regexp-match #rx"StructureDefinition-PinnedObservation.json" out)
  (check-regexp-match #rx"http://hl7.org/fhir/StructureDefinition/Patient[|]4[.]0[.]1"
                      (file->string (build-path dir "pinned-out" "StructureDefinition-PinnedObservation.json"))))

(test-case "a `|` list of Reference targets is an error at its rule, exit 1"
  (project! "piped" (string-append "Profile: PipedObservation\nParent: Observation\n"
                                   "* subject only Reference(Patient | Group)\n"))
  (define-values (code out err) (fsh "piped" "--fhir-version" "4.0.1" "-o" "piped-out"))
  (check-equal? code 1)
  (check-regexp-match #rx"a[.]fsh:3:3: fsh lint error: Using '[|]' to list references is no longer supported" err))

(test-case "an unquoted Target is an error at the word, exit 1"
  (project! "target" (string-append "Profile: P\nParent: Patient\n\n"
                                    "Mapping: M\nSource: P\nTarget: http://example.org/target\n* name -> \"x\"\n"))
  (define-values (code out err) (fsh "target" "--fhir-version" "4.0.1" "-o" "target-out"))
  (check-equal? code 1)
  (check-regexp-match #rx"a[.]fsh:6:9: fsh lint error: The Target of a Mapping is a quoted string: write `Target: \"http://example.org/target\"`" err))

(delete-directory/files dir)
