# fsh — FHIR Shorthand

A compiler for [FHIR Shorthand](https://hl7.org/fhir/uv/shorthand/), the language SUSHI compiles, and
its reverse, FHIR back to FSH. It compiles real Implementation Guides from their own FSH with no
errors; [TESTING.md](TESTING.md) has how closely the output matches what HL7 published.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

[Build the family's tools](https://github.com/oloveluck/fhir#getting-the-tools) once and put
`dist/bin` on your `PATH`; they run without Racket. With Racket installed, `raco fsh` is the same.

```sh
fsh patient.fsh -o out/                  # a file or a folder, to FHIR JSON
fsh my-ig/                               # a project with a sushi-config.yaml, into my-ig/fsh-generated
fsh my-ig/ --pack my-ig.tgz              # … and a FHIR package
fsh check my-ig/                         # validate every example against the project's profiles
fsh --decompile StructureDefinition-my-patient.json   # FHIR JSON back to FSH
```

Problems point at the rule they come from, with exit status 1:

```
$ fsh broken.fsh
broken.fsh:3:3: fsh compile error: profile: Broken: `nmae` is not an element of Patient
 3 | * nmae 1..1
   |   ^^^^
```

## In Racket

A `.fsh` file that starts with `#lang fsh` is a module; requiring it registers its profiles and value
sets by canonical, so the validator finds them:

```racket
#lang racket/base
(require "patient.fsh")
(for/list ([r compiled]) (list (hash-ref r 'resourceType) (hash-ref r 'id)))
;; '(("StructureDefinition" "my-patient") ("ValueSet" "favorite-colors") ("Patient" "ExamplePatient"))
```

## Develop

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx ../fhir/fhir-validation .   # repos side by side
raco test .
```

Reference: [oloveluck.github.io/fsh](https://oloveluck.github.io/fsh/). The language for agents,
with a checked cheat sheet and the common problems: [guide.md](guide.md); the workflow without MCP:
[.claude/skills/fsh](.claude/skills/fsh/SKILL.md). Architecture: `CLAUDE.md`.
License: MIT.
