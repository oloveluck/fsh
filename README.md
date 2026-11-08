# fsh — FHIR Shorthand

A from-scratch compiler for [FHIR Shorthand](https://hl7.org/fhir/uv/shorthand/), the language SUSHI
compiles, and its reverse (FHIR to FSH, as GoFSH does): profiles, extensions, logical models,
instances, value sets, code systems, rule sets, mappings and slicing, for R4 or R5. Measured on real
Implementation Guides, each compiled from its own FSH and compared with its published package:

| IG | Compile errors | Resources identical to the published package |
|---|---|---|
| International Patient Summary 2.0.1 | 0 | 94 of 114 (108 with all published content) |
| mCODE 4.0.0 | 0 | 311 of 349 (339 with all published content) |
| Da Vinci Plan-Net 1.2.0 | 0 | 84 of 109 (99) |
| Da Vinci HRex 1.1.0 | 0 | 31 of 46 (46) |
| Genomics Reporting 2.0.0 | 0 | 113 of 141 (129) |

The published IPS and mCODE packages also round-trip through the decompiler: all 117 IPS resources
and 341 of 349 mCODE resources come back identical. SUSHI's own importer tests are the oracle for the
parser's diagnostics: 514 of 515 verdicts agree, with no missed errors. `CLAUDE.md` lists the known gaps.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

`fsh` is one of the family's command-line tools, which run without Racket:
[build them once](https://github.com/oloveluck/fhir#getting-the-tools) and put `dist/bin` on your
`PATH`. With Racket and the packages installed, `raco fsh` runs the same commands.

```sh
fsh patient.fsh -o out/                  # one file, or a folder (recursively), to FHIR JSON
fsh my-ig/                               # a project with a sushi-config.yaml: into my-ig/fsh-generated
fsh my-ig/ --pack my-ig.tgz              # … and a FHIR NPM package
fsh patient.fsh --snapshot -o out/       # with snapshots
fsh check my-ig/                         # every example validated against the project's profiles
fsh --decompile StructureDefinition-my-patient.json   # FHIR JSON back to FSH
```

A project's `sushi-config.yaml` gives the canonical, the FHIR version and the dependencies, which are
looked up as `<id>-<version>.tgz` among the packages. Without one, `--fhir-version 4.0.1|5.0.0` and
`--canonical <url>` say the same. `fsh check --capability <id>` also checks a server generated from the
project against one of its CapabilityStatements, and a project's `.map` files (FHIR Mapping Language)
are checked against its structures.

Problems are positioned at the rule they come from, and the exit status is 1. `--format json` or
`--format outcome` (an OperationOutcome) give them as data:

```
$ fsh broken.fsh
broken.fsh:3:3: fsh compile error: profile: Broken: `nmae` is not an element of Patient
 3 | * nmae 1..1
   |   ^^^^
```

`fsh check` positions a validation problem in an example the same way, at the rule that wrote the
element, and names the profile rule that constrains it.

## In Racket

`#lang fsh` makes a `.fsh` file a module. Requiring it compiles the FSH and registers its profiles,
value sets and code systems by canonical, so the validator and other modules resolve them:

```racket
#lang racket/base
(require "patient.fsh")           ; patient.fsh starts with `#lang fsh`
(for/list ([r compiled]) (list (hash-ref r 'resourceType) (hash-ref r 'id)))
;; '(("StructureDefinition" "my-patient") ("ValueSet" "favorite-colors") ("Patient" "ExamplePatient"))
```

The module provides `document` (the FSH syntax tree), `compiled` (FHIR JSON) and `resources` (typed
FHIR values). `racket patient.fsh` prints the JSON. As libraries: `fsh->fhir` (`fsh/compile`),
`fhir->fsh` (`fsh/decompile`), `compile-project` and `pack-project!` (`fsh/project`, `fsh/pack`), and
`check-fsh-project` (`fsh/check`).

## Develop

From source, with the family's repositories side by side:

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx ../fhir/fhir-validation .
raco test .          # unit tests and the IG, golden and importer corpora
```

Reference documentation: [oloveluck.github.io/fsh](https://oloveluck.github.io/fsh/). Architecture
notes: `CLAUDE.md`; the family's conventions and conformance table are in the fhir repository's
`CLAUDE.md`. License: MIT (vendored test corpora keep their own terms, see `tests/vendor`).
