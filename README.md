# fsh — FHIR Shorthand for Racket

A from-scratch FHIR Shorthand compiler (the SUSHI authoring language) and its reverse (GoFSH-style FHIR → FSH): profiles, extensions, logical models, instances, value sets, code systems, rule sets, mappings and slicing, for R4 or R5. Measured on real IGs: the International Patient Summary 2.0.1, mCODE 4.0.0, Da Vinci Plan-Net 1.2.0 and Da Vinci HRex 1.1.0 compile from their own FSH sources with no errors, and their published packages come out resource for resource (IPS 84 of 114 identical, 106 with all published content; mCODE 225/329 of 349; Plan-Net 78/94 of 109; HRex 20/40 of 46); `CLAUDE.md` lists the known gaps.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `fsh`. `#lang fsh` — a `.fsh` file is a module; `racket file` prints the compiled JSON.
Reference: [oloveluck.github.io/fsh](https://oloveluck.github.io/fsh/).

## Install

```sh
raco pkg install fsh          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx ../fhir/fhir-validation .
```

Depends on `fhir` (the R5 tower; snapshots additionally need `fhir-spec`), `fhirpath`.

## Use

```racket
(require fsh/compile fsh/decompile)   ; fsh->fhir : string -> (listof jsexpr) ; fhir->fsh : jsexpr ... -> string
```

```sh
raco fsh input/fsh/ -o out/      # compile a folder SUSHI-style, recursively (or one file)
raco fsh input/fsh/ --fhir-version 4.0.1 --canonical http://example.org/fhir   # an R4 IG
raco fsh profile.fsh --snapshot  # also emit snapshots (needs: raco pkg install fhir-spec)
raco fsh --decompile StructureDefinition-x.json
raco fsh check ig/               # examples against the profiles, and the project's .map files
                                 # (FHIR Mapping Language, needs fml) against its structures
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
