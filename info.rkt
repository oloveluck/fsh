#lang info

(define collection "fsh")
(define version "0.3")
(define pkg-desc "FHIR Shorthand for Racket: a bidirectional FSH compiler (FSH -> FHIR and GoFSH-style FHIR -> FSH), #lang fsh, and raco fsh — including `raco fsh check`, which validates a project's examples against its profiles and positions every problem in the FSH")
(define pkg-authors '(oloveluck))
(define license 'MIT)
(define deps '("base" "fhir" "fhirpath" "fhir-tx" "fhir-validation" "brag-lib" "br-parser-tools-lib"))
;; fhir-spec (the core and dependency packages) is needed for real projects and the IG tests; fhir-server
;; only for `raco fsh check --capability` (loaded on demand)
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/fsh.scrbl" ())))
(define raco-commands '(("fsh" (submod fsh/cli main) "compile FSH to FHIR JSON (SUSHI-style), check a project, decompile FHIR to FSH" #f)))
(define test-omit-paths '("tests/vendor" "scribblings"))
