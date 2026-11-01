# fsh — FHIR Shorthand for Racket

A from-scratch FHIR Shorthand compiler (the SUSHI authoring language) and its reverse (GoFSH-style FHIR → FSH): profiles, extensions, logical models, instances, value sets, code systems, rule sets, mappings and slicing, targeting R5. Golden corpus 22/22 forward and round-trip.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `fsh`. `#lang fsh` — a `.fsh` file is a module; `racket file` prints the compiled JSON.

## Install

```sh
raco pkg install fsh          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir .
```

Depends on `fhir` (the R5 tower; snapshots additionally need `fhir-spec`), `fhirpath`.

## Use

```racket
(require fsh/compile fsh/decompile)   ; fsh->fhir : string -> (listof jsexpr) ; fhir->fsh : jsexpr ... -> string
```

```sh
raco fsh input/fsh/ -o out/      # compile a folder SUSHI-style (or one file)
raco fsh profile.fsh --snapshot  # also emit snapshots (needs: raco pkg install fhir-spec)
raco fsh --decompile StructureDefinition-x.json
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
