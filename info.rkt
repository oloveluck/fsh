#lang info

(define collection "fsh")
(define version "0.3")
(define pkg-desc "FHIR Shorthand for Racket: a bidirectional FSH compiler (FSH -> FHIR and GoFSH-style FHIR -> FSH), #lang fsh and raco fsh")
(define pkg-authors '(oloveluck))
(define license 'MIT)
(define deps '("base" "fhir" "fhirpath" "brag-lib" "br-parser-tools-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands '(("fsh" (submod fsh/cli main) "compile FSH to FHIR JSON (SUSHI-style); decompile FHIR to FSH" #f)))
(define test-omit-paths '("tests/vendor"))
