---
name: fsh
description: Write, check, compile and package FHIR Shorthand (FSH) without an MCP server — `raco fsh check` (compile + validate every example, problems positioned at their FSH rules), `raco fsh` (FHIR JSON out), `--pack` (an NPM package), GoFSH (`--decompile`, FHIR -> FSH), and `#lang fsh` modules. Use when writing or debugging profiles, extensions, value sets, code systems, logical models or example instances in .fsh files.
---

# FSH without MCP

Read [`guide.md`](../../../guide.md) first: the language in brief, a cheat sheet and a worked
example (both checked by the tests), and the common problems with the messages they produce (that
guide is what fhir-mcp serves as `learn://fsh`). This skill is the workflow around it.

`fsh` and `raco fsh` are the same tool (the standalone build is `dist/bin/fsh`).

## The loop
1. **Write** the `.fsh` files of one project in a directory (a `sushi-config.yaml` beside them, or
   in the parent, gives the canonical, FHIR version and dependencies, as SUSHI reads it).
2. **Check.** `raco fsh check dir/` compiles the project and validates every example instance
   against its base type and the profiles it claims. Each problem is at its rule, and a validation
   problem names the profile rule that constrains it:
   ```
   p2/a.fsh:8:3: fsh validate error: instance prelim: Observation.status: value does not match required pattern "final" (constrained by profile Final at line 4)
    8 | * status = #preliminary
      |   ^^^^^^
   fsh check: 1 examples, 1 with errors; 1 error(s), 0 warning(s)
   ```
   Exit 1 on an error, 0 otherwise. `--format json` gives the same diagnostics on stdout
   (`source`, `line`, `column`, `message`, `phase`: parse, compile or validate); `--format outcome`
   as an OperationOutcome. `--capability <CapabilityStatement id>` also reports what a server
   generated from the project would leave unmet (needs fhir-server, and a sushi-config.yaml: the
   server is generated from the project packed as a package).
3. **Fix at the rule** the message points to, and check again. Parse errors stop the compile;
   compile errors (an unknown element, an unknown `Parent`, a duplicate id) are each reported and
   the rest still compiles; validation errors are the examples against their profiles.

## Output
- `raco fsh dir/ -o out/` — the resources as FHIR JSON files, one per resource (default
  `dir/fsh-generated`). `--fhir-version 4.0.1|5.0.0`, `--canonical <url>` when there is no
  sushi-config; `--snapshot` adds snapshots.
- `raco fsh dir/ --pack out.tgz` — a FHIR NPM package (package.json from sushi-config.yaml,
  conformance resources in `package/`, examples in `package/example/`), servable by
  `raco fhir-server serve-ig out.tgz`. Needs a sushi-config.yaml (its id; version 0.1.0 when it
  gives none; without `out.tgz`, `<id>-<version>.tgz` in the current directory).
- `raco fsh --decompile resource.json …` — GoFSH: FHIR JSON to FSH. The output compiles back to
  the same resource (the published IPS and mCODE packages round-trip); it writes `^caret` rules
  where a FSH shorthand would lose something.

## From Racket
A file that starts with `#lang fsh` is a module:
```racket
#lang racket/base
(require "bp.fsh")                 ; compiles when this module compiles
(for ([r compiled]) (printf "~a/~a\n" (hash-ref r 'resourceType) (hash-ref r 'id)))
```
It provides `compiled` (FHIR JSON), `resources` (typed nodes) and `document` (the AST), and
registers its profiles, value sets and code systems, so validation (`fhir/conformance/operations`'
`$validate`) and other modules find them by canonical. `racket bp.fsh` prints the JSON. For a
whole project: `(check-fsh-project dir)` (`fsh/check`, the same report as `raco fsh check`) and
`(compile-project dir)` (`fsh/project`).

`raco make` compiles a `#lang fsh` module but does not validate its examples (that needs the whole
project and the packages). To hold them to the profiles on every `raco test`, keep a test beside them:
```racket
#lang racket/base
;; the FSH project's examples, checked by `raco test` as `raco fsh check fsh/` would
(module+ test
  (require rackunit racket/runtime-path
           (only-in fsh/check check-fsh-project check-report-diagnostics)
           (only-in fhir/diagnostic diagnostic->string diagnostic-severity))
  (define-runtime-path dir "fsh")
  (define errors (filter (lambda (d) (eq? (diagnostic-severity d) 'error))
                         (check-report-diagnostics (check-fsh-project dir))))
  (check-equal? (map diagnostic->string errors) '()))
```
A failing example shows as `fsh/p.fsh:10:1: fsh validate error: instance bad: Patient.birthDate:
minimum required = 1, but found 0 (constrained by profile P at line 4)`. An example that is MEANT to
fail (to show a profile refusing something) belongs in a directory of its own (`fsh-invalid/`, with
the profile's file copied or linked beside it): check it there and assert its exact diagnostic, so the
project's own `raco fsh check fsh/` stays a pass/fail gate. A misspelt element in a profile
(`* birthdate 1..1`) already fails `raco make` of the `#lang fsh` file, at its line.

## Working on the compiler itself
`CLAUDE.md` is the architecture. `raco test .` runs everything: the SUSHI golden corpus, five real
IGs' own FSH against their published packages (IPS, mCODE, Plan-Net, HRex, Genomics Reporting),
GoFSH round trips, and SUSHI's importer tests; `TESTING.md` has the floors, which must not drop.
`tests/guide-test.rkt` keeps guide.md's examples and messages true.
