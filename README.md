# fhir-fsh

FHIR Shorthand for Racket: a bidirectional FSH compiler (FSH -> FHIR and GoFSH-style FHIR -> FSH) and #lang fhir/fsh.

Part of **fhir — FHIR as a family of Racket `#lang`s** (main repo: `oloveluck/fhir`). This package
contributes `fsh/` to the shared `fhir` collection. `#lang fhir/fsh`.

## Depends on

`fhir-core`, `fhir-fhirpath`
(Racket packages; link them first) plus `brag-lib`, `br-parser-tools-lib`.

## Develop

```sh
raco pkg install --link --auto .     # link into the `fhir` collection
raco test fsh/
```

The `raco fhir` CLI and the conformance-suite runners are driven from the main `fhir` package:

```
raco fhir fsh <dir>, raco fhir gofsh <file.json>   (batch compile SUSHI-style; decompile)
```

Architecture notes: `CLAUDE.md` here; conventions and the ratchet table: the main repo's `CLAUDE.md`.
