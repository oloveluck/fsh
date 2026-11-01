# CLAUDE.md — fsh

Conventions, HTDP design style, and the conformance ratchet table live in the main `fhir` repo's
CLAUDE.md (`~/Projects/fhir`); this file holds only the notes for this package. This repo root IS the
`fsh` collection (`(define collection "fsh")`): modules are `fsh/compile` etc., and imports of the
foundation use collection paths (`fhir/model/node`, never a relative path into another package).
The CLI is `cli.rkt` (`raco fsh`); the conformance runner(s) live in `tests/` behind `--tck`.

## FHIR Shorthand lang (`#lang fsh`, `fsh/`)
A from-scratch FSH compiler (the SUSHI authoring DSL) built onto this repo's model. Layers: `parser/` (a `#lang brag` grammar + a stateful `br-parser-tools` lexer
that emulates FSH's ANTLR pushMode/popMode via a mode-stack box + one lexer per mode; prefab AST with
`lower-entity`; `path.rkt` sub-parses FSH paths; `parse.rkt` pre-splits a doc into entity chunks since
brag's start rule is one `entity`), `resolve.rkt` (walks an FSH path against the R5 registry via
`model/introspect` `full-field-specs` → element type code / cardinality kind — for fixed/pattern key
selection and Instance array nesting), `caret.rkt` (get/set at a caret path), `compile.rkt`
(entity AST → **FHIR jsexpr**). Targets **R5** (`fhirVersion "5.0.0"`, canonical base default
`http://example.org`). Profiles/Extensions → a StructureDefinition **differential** (type-qualified
paths, per-element `id`; **no snapshot** — deferred); `=` assigns `patternX`, `= … (exactly)` assigns
`fixedX` (both prim+complex); root `obeys` lands a constraint on the base-type element sourced to the
profile url. Instance → a plain resource (schema-driven nesting; `meta.profile` stamped when
`InstanceOf` is a local profile). ValueSet/CodeSystem → compose/concept (system pulled from `from
system` or the code token; CodeSystem gets `count`). Whole-doc compile resolves `Alias`es and expands
non-parameterized `insert RuleSet` first. `#lang fsh` (`fsh/base.rkt` custom `#%module-begin` +
`fsh/lang/reader.rkt`'s `#:read-syntax` running `parse-fsh-doc`, emitting `(fsh-entity <ast>)` forms)
makes a `.fsh` file a module providing `document`/`compiled`(jsexpr)/`resources`(typed nodes via
`from-json`), printing JSON in `main`. `raco fhir fsh <dir>` batch-compiles a folder SUSHI-style.
**Bidirectional**: the reverse direction (FHIR → FSH, "GoFSH") is `fsh/decompile.rkt` +
`fsh/print.rkt` (FSH AST → text). `fhir->fsh` (jsexpr) / `node->fsh` (typed node) emit idiomatic rules
where recognised (card/flag/binding/only-with-targetProfile/obeys/concept/include-exclude) and a
lossless `^caret` catch-all for everything else; Invariants are **not** emitted standalone (SUSHI-style)
but re-synthesised from SD constraints so a recompile refills severity/human/expression. `raco fhir
gofsh <file.json>` is the CLI. Invariants are consumed only as SD constraint sources — `compile-doc`
skips standalone `fsh-invariant` output.

## Real-IG corpus: IPS 2.0.1 (`tests/ig-ips-test.rkt`, `tests/ig-harness.rkt`)
The International Patient Summary IG's own FSH sources (`tests/vendor/ips-2.0.1`, HL7/fhir-ips at tag
2.0.1, 123 files / 274 entities) are compiled for R4 and compared with the PUBLISHED package
(fhir-spec's `hl7.fhir.uv.ips.tgz` — SUSHI + IG Publisher output for the same sources), paired by
(resourceType, id) after normalising publisher-only content (narrative, snapshots, dates, config
metadata, version pins). Current: **0 compile errors; 114 resources paired; 78 exact; 84 with all
published content; 324/440 differential elements** — ratcheted. This is the honest conformance
number; the 22 golden pairs below are hand-authored and far easier.
It drove these fixes: STAR lexing after trailing whitespace / blank lines; `40` lexed as TIME;
keyword names in paths (`system`); triple-quoted strings (trim + dedent); unknown string escapes
kept verbatim; per-entity error isolation (`compile-doc/errors`); `Usage: #inline` instances embedded,
not emitted; numeric-looking instance names; InstanceOf/Parent by name, id or url and profile
chains; local parents' canonical + base type; inherited `mapping`; extensions on primitives
(`_display`); soft indexing per path prefix and named slices in instances; aliases in codes;
`Reference(name)` -> `Type/id` (ids from `* id =` rules too, first wins); schema-aware carets
(`caret.rkt` typed mode: repeating elements, `[+]`/`[=]`, `extension[url]`, typed values); ValueSet
filters / valueSet includes / versions / per-system concept merging; local names in bindings,
`only` types and target profiles; root `.` paths; rule-by-rule parsing of long entities (IPS's
Composition profile: 105 s -> instant). `fsh-fhir-version` (and `raco fsh --fhir-version 4.0.1`)
selects the target version; directories compile recursively.
Also: bare names of core/dependency CodeSystems and ValueSets resolve through the SD index's canonical
lookup (`from system ConditionCategoryCodes`, bindings to `SNOMEDCTBodyStructures`); an instance's
named slices take positions in the InstanceOf profile's declaration order (local profiles, inherited
through Parent chains).
Known gaps (each a measurable diff in the corpus): ActorDefinition->Basic conversion and the
ImplementationGuide resource (publisher/config, out of scope); some slicing/constraint details in
differentials (116 elements).

Conformance: `fsh/tests/golden-test.rkt` runs the vendored SUSHI **golden corpus** (`.fsh`+`.json`
pairs) through a tolerant deep-subset comparator (expected ⊆ actual; list items matched
content-independently so differential `element` arrays line up by path) — forward **22/22**;
`fsh/tests/roundtrip-test.rkt` checks a **FSH→FHIR→FSH→FHIR fixpoint** (deep-equal, **22/22** — all
pairs) and a parse∘print∘parse AST fixpoint.

The initially-deferred features are all implemented: **CodeSystem concept hierarchy** (`* #a #b` nests,
`count` totals the tree); **Extension auto-scaffolding** (`add-extension-scaffold`/`add-extension-header`
— root short/definition, `extension` 0..0 / min + per-slice `.extension` 0..0 + `.url` fixedUri, `url`
fixedUri, `context` + type-characteristics); **discriminator slicing** (`contains` sets base `min` =
Σ slice mins, slice ids `path:slice`, slicing authored via `^slicing…` carets; `element-loc` maps FSH
paths → ElementDefinition id/path, `[x]`→`[x]`, `[slice]`→`:slice`); **Logical models & custom
Resources** (`compile-logical-resource` — root element at the entity id, `addElement`/kind/derivation=
specialization); **parameterized RuleSets** (`instantiate-ruleset` substitutes `{param}` in the raw
body tokens and re-parses); **snapshot generation** (opt-in `emit-snapshot?` → `sd/snapshot.rkt`, a
ProfileUtilities-style merge of the differential onto the base's snapshot from the R5 core package via
`sd/sd-index.rkt`); and **Mapping** (`apply-mapping` — a post-pass injecting `StructureDefinition.mapping`
+ `ElementDefinition.mapping` into the named Source SD). `fsh/tests/features-test.rkt` covers the
golden-less features (param RuleSets, snapshot, Mapping). The reverse direction is complete too:
`decompile-extension` emits minimal FSH and lets the forward compiler regenerate the scaffold, and the
profile decompiler is slice-aware (`id->fsh-rel` inverts id↔path, reconstructs `contains` + `^slicing`
carets) — so all 22 pairs round-trip. **Reslicing** (a slice of a slice — ids join with `/`, e.g.
`component:Lab/Chem`; `element-loc`'s `id-seg` and the `contains` handler are reslice-aware, as is the
decompiler's `base-id-of`) and **multi-discriminator** slicing both compile and round-trip
(`fsh/tests/features-test.rkt`). The golden pairs are HAND-AUTHORED to match SUSHI's documented
output; the IPS corpus above is the real measure, and it shows the gaps that remain.
