#lang racket/base

;;; pack-test.rkt — `raco fsh <dir> --pack`: an FSH project (mCODE 4.0.0's own sources) compiled by
;;; this compiler and written as a FHIR NPM package. Checks the package.json taken from
;;; sushi-config.yaml (id, version, fhirVersions, canonical, dependencies + the core package) and the
;;; layout: conformance resources and #definition instances in package/, examples in
;;; package/example/, a .index.json in each. (fhir-server/tests/fsh-ig-server-r4-test.rkt in the fhir repo serves it.)

(require rackunit racket/runtime-path racket/file racket/list racket/string json file/gunzip file/untar
         (only-in fhir/spec-package spec-package-path)
         fsh/project)

(define-runtime-path mcode "vendor/mcode-4.0.0/fsh")

;; package-entries : tgz -> (hash name -> bytes)
(define (package-entries tgz)
  (define out (make-hash))
  (define-values (pin pout) (make-pipe))
  (define t (thread (lambda () (call-with-input-file tgz (lambda (i) (gunzip-through-ports i pout))) (close-output-port pout))))
  (untar pin #:handle-entry (lambda (k n c sz a) (when (eq? k 'file) (hash-set! out (path->string n) (read-bytes sz c))) '()))
  (thread-wait t)
  out)

(cond
  [(not (spec-package-path "hl7.fhir.us.core-6.1.0.tgz" #:optional? #t))
   (printf "  FSH pack: skipped (needs fhir-spec with mCODE's dependencies)\n")]
  [else
   (define p (compile-project mcode))
   (check-equal? (project-diagnostics p) '())
   (define tgz (make-temporary-file "mcode-~a.tgz"))
   (pack-project! p tgz)
   (define es (package-entries tgz))
   (define pj (bytes->jsexpr (hash-ref es "package/package.json")))
   (check-equal? (hash-ref pj 'name) "hl7.fhir.us.mcode")
   (check-equal? (hash-ref pj 'version) "4.0.0")
   (check-equal? (hash-ref pj 'fhirVersions) '("4.0.1"))
   (check-equal? (hash-ref pj 'canonical) "http://hl7.org/fhir/us/mcode")
   (check-equal? (hash-ref (hash-ref pj 'dependencies) 'hl7.fhir.us.core) "6.1.0")
   (check-equal? (hash-ref (hash-ref pj 'dependencies) 'hl7.fhir.r4.core) "4.0.1")
   (define names (hash-keys es))
   (define (count-of rx) (count (lambda (n) (regexp-match? rx n)) names))
   (check-equal? (count-of #rx"^package/StructureDefinition-") 53)
   (check-equal? (count-of #rx"^package/CapabilityStatement-") 10)        ; Usage: #definition instances
   (check-true (>= (count-of #rx"^package/example/") 180))
   (check-true (hash-has-key? es "package/.index.json"))
   (check-true (hash-has-key? es "package/example/.index.json"))
   (define idx (bytes->jsexpr (hash-ref es "package/.index.json")))
   (check-true (for/or ([f (hash-ref idx 'files)]) (equal? (hash-ref f 'url #f) "http://hl7.org/fhir/us/mcode/StructureDefinition/mcode-cancer-patient")))
   (printf "  FSH pack: mCODE 4.0.0 -> ~a files in the package\n" (length names))
   (delete-file tgz)])
