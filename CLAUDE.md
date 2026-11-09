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
`from-json`), printing JSON in `main`. `raco fsh <dir>` batch-compiles a folder SUSHI-style.
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
metadata, version pins). Current: **0 compile errors; 114 resources paired; 94 exact; 108 with all
published content; 438/438 differential elements** — ratcheted. This is the honest conformance
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
Rule-level diagnostics (`compile-doc/diagnostics`, used by `project.rkt`): every rule carries its
position, and a path whose first element the base type does not have is an error at that rule; all of
them are reported, and the entity still compiles.
Also: bare names of core/dependency CodeSystems and ValueSets resolve through the SD index's canonical
lookup (`from system ConditionCategoryCodes`, bindings to `SNOMEDCTBodyStructures`); an instance's
REQUIRED named slices take positions in the InstanceOf profile's declaration order (local profiles,
inherited through Parent chains), the others in first-mention order.
Differential SHAPE now follows SUSHI: elements in base-snapshot order (slices grouped under their
element; `snapshot-order`); a type-specific choice path (`valueQuantity`) is the type slice
`value[x]:valueQuantity` (min 0, base max, that one type) under a `value[x]` with type slicing
($this/type); `extension contains X named y` slices `extension` by url and types the slice
`Extension` + profile; an `only` that narrows nothing emits no element, and `only` types keep the base
order; `only <profile url>` takes the profile's base type as its code; a profile's `kind` is its base's
(complex-type for datatype profiles); aliases resolve in SD `=` values; Invariant fields may be rules
(`* expression = "…"`).
Known gaps (each a measurable diff in the corpus): ActorDefinition->Basic conversion and the
ImplementationGuide resource (publisher/config, out of scope); 45 differential elements (a few
`min`s, one slice-child order, logical-model root `short`); 5 Bundle narratives.

## Real-IG corpus 2: mCODE 4.0.0, with IG dependencies (`tests/ig-mcode-test.rkt`)
mCODE's own FSH (`tests/vendor/mcode-4.0.0`, HL7/fhir-mCODE-ig at tag 4.0.0, 57 files) against the
published `hl7.fhir.us.mcode-4.0.0.tgz`. mCODE builds on US Core 6.1.0 and Genomics Reporting 2.0.0:
`fsh/ig-config.rkt` reads `canonical`/`fhirVersion`/`dependencies` from sushi-config.yaml and locates
each dependency in fhir-spec (`<id>-<version>.tgz`); the packages go ahead of core in
`current-sd-packages`. `raco fsh <dir>` does the same when it finds a sushi-config.yaml (next to the
sources or up to two levels up); flags override it, missing dependencies are reported.
Current: **0 compile errors; 349 paired; 311 exact; 339 with all published content; 472/473 differential
elements** — ratcheted. It drove: profiles of DEPENDENCY packages for Parent and InstanceOf (`external-sd`:
base type, canonical, `meta.profile`), aliases on InstanceOf and along Parent chains; SUSHI's required
values (`assign-required`: an element the InstanceOf profile requires with fixed[x]/pattern[x] is
assigned when absent; a required slice gets its pattern; a named slice the instance uses gets its
required children and, for extensions, its `url` — constraints from the profile chain, local
differentials over the dependency's snapshot); indented rules (FSH 2.0 path context, `parse.rkt`
`apply-indentation`: two spaces per level, a context's `[+]` reads `[=]` for its children, concept rules
take the parent's codes; path-only rules advance soft indices); parameterized RuleSet bodies re-texted
with their quotes, codes and STAR lines intact; RuleSets parsed rule by rule (their header alone is not a
parse); a bare four-digit `6000` is a NUMBER (FSHLexer.g4), stringified when assigned to a string-typed
primitive; `descendant-of` filters emit FHIR's `descendent-of`; an Extension that sets `^context` gets no
default Element context; `Usage: #definition` canonical resources get `<canonical>/<Type>/<id>` as url.
The harness also drops the OIDs the IG Publisher assigns to conformance resources.
Required values follow SUSHI recursively (`required-entry`): an element's pin completed by its
required children, a child's required named slices as its entries (a list when the path repeats, from
the element's max or the tower); `min` inside a slice falls back to the unsliced element's; a profile
chain ending at a base type contributes the base snapshot (Observation.code is 1..1 there, not in the
profile); a named slice an instance uses gets the slice's own pattern (`identifier:NPI`'s system).
Also: `System#code` assigned to a `code` keeps only the code; once a profile narrows `value[x]` to
one type, `valueQuantity` addresses `value[x]` itself (no type slice); an instance's REQUIRED slices
take positions in profile order, the rest in first-mention order, and a new slice takes the next
unused position (re-mentioning an earlier slice used to rewind the counter, so two slices shared an
entry). These came from the FSH -> package -> server loop (below): 250 -> 283/283.
Remaining (measured): 28 profiles whose differential lacks elements SUSHI writes (mostly genomics and
performance-status profiles), a few instance details.

## Real-IG corpus 3: Da Vinci PDex Plan-Net 1.2.0, its oracle fetched (`tests/ig-plannet-test.rkt`, `tests/ig-fetch.rkt`)
Plan-Net's own FSH (`tests/vendor/plannet-1.2.0`, HL7/davinci-pdex-plan-net at tag 1.2.0, 9 files /
119 entities, CC0) against its published package. Neither the package nor its dependency is in
fhir-spec: `ig-fetch.rkt` downloads a package once from packages.fhir.org into `tests/vendor/packages/`
(gitignored), verifies it against a pinned sha1 (or the registry's own shasum, printed so the test can
pin it), and returns #f offline (the test skips with a note; CI has the network). sushi-config declares
`hl7.fhir.us.core.v311 3.1.1`: on the registry that is a VERSION-ALIAS STUB — a package.json depending
on `hl7.fhir.us.core 3.1.1` and nothing else — which the fetcher follows (`stub-dependency`) to the real
package; the test pins the real one. US Core 7.0.0, 6.1.0 and HRex are declared too but no rule reaches
them. Current: **0 compile errors; 109 paired (the CapabilityStatement, SearchParameters and IG are not
FSH-authored); 84 exact, 99 with all published content; 321/321 differential elements** — ratcheted.
It drove: a code's system part as a CodeSystem NAME (`LanguageProficiencyCS#30`, this document's or a
dependency's — `resolve-system`); `[slice][k]` as the k-th slot of a slice (base+k while no other slice
holds it, else the next unused position — `daysOfWeek[5]` written after `allDay` lands after it);
a type slice below a nested extension (`extension[x].value[x]:valueCodeableConcept`) typed through
`Extension.extension`'s datatype (`base-element` dives); a RuleSet argument ending in an escaped paren
(`Canonical(TG263CS\)`) kept whole; profiles of a dependency's profile carry the BASE resource's core
mapping declarations (SUSHI), not the Parent's; a value assigned to a slice's value/pattern
discriminator element makes it required (`identifier[displayName].use = #usual` -> min 1,
`require-discriminator`; mCODE's RadiotherapyVolume). A nested extension's sub-slices take the required ones first too, in the
Extension's own declaration order (`declared` follows the context's slices into their Extensions;
HansSoloRole1: `code`, `status`, then the optional `issuer`). `OrgPeriod named org-period` resolves to
`organization-period` through the extensions package SUSHI loads implicitly: `ig-config.rkt`'s
`implicit-packages` puts the newest `hl7.fhir.uv.extensions.<r4|r5>` and `hl7.terminology.<r4|r5>`
found in `current-package-directories` or fhir-spec into every project's SD context (project.rkt),
and the Plan-Net and HRex tests fetch extensions 5.2.0 (pinned). Remaining: 31 resources differ in
details the CI output names (instance details, a few `min`s).

## Real-IG corpus 4: Da Vinci HRex 1.1.0 (`tests/ig-hrex-test.rkt`)
Health Record Exchange's own FSH (`tests/vendor/hrex-1.1.0`, HL7/davinci-ehrx at tag 1.1.0, 46 files /
99 entities, CC0) against its published package (fetched, pinned). It declares THREE US Core
versions (7.0.0, and 6.1.0 / 3.1.1 as `.v610`/`.v311` aliases; 6.1.0 from fhir-spec, the others
fetched) and pins Parents and Reference targets to one (`Parent: USCoreProvenance|7.0.0`,
`Reference(USCorePatientProfile|7.0.0)`); the packages go into the SD context in the declared order.
Current: **0 compile errors; 46 paired (a Basic, a Binary, the IG and one JSON-kept SD are not
FSH-authored); 31 exact, 46 with all published content; 138/138 differential elements** — ratcheted.
It drove: a Reference target by package NAME with its `|version` kept on the canonical
(`target-urls`); a parameterized RuleSet inserting another parameterized RuleSet (the nested
reference re-texts with its paren — HRex's concept maps, 500 `insert map…` rows); a failing
expansion is one entity's error; an embedded resource typed by the `resourceType` its rules set first
(`* parameter[x].resource` + `* resourceType = "Coverage"` + the Coverage's elements, shaped by its
schema — `ftype->type`); `multipleOr.extension[$alias]` under a primitive (`_multipleOr`) gets the
alias's url (`json-key`); a caret on a concept (`* #code` + indented `^property[0].code`); carets with
aliased urls (`^extension[$compliesWithProfile]`); `1..` / `..3` keep the other bound; `valueIdentifier
1..` makes value[x] required as well, and the type-slice defaults stop at the slice (its children got
min 0 / max 1 before); an assigned discriminator gets min 1 only where the base is optional
(Parameters.parameter.name is 1..1 — SUSHI writes what changes); `^binding.extension…` on an element
the Parent binds starts from the inherited binding; `Reference(X)` to a contained instance is `#id`;
the harness `#:dump`s named resources' (id min max; the whole normalised JSON of other resources).
Required values reach further, as SUSHI's do: a type slice's pattern (`valueIdentifier.type = UMB`
fills `valueIdentifier.type`), an embedded resource's profile (`parameter[Consent].resource only
HRexConsent` fills Consent.category) and the slices an embedded resource USES
(`fill-embedded-profiles`, positions from `embedded-slice-positions`). Two SUSHI index rules:
a bare `[+]`/`[=]` on an array has its OWN counter, independent of the named slices (`class[+]`
after `class[group]` is element 0 — mCODE's bundle says "the first unnamed slice entry must be
numbered"), and a slice's pattern MERGES with what the instance wrote, the pattern's codings first
(`merge-pattern`: member-match-in-provider's `[group, subplan]`). Remaining: one resource (named in
the CI output).

## Projects and packages (`fsh/project.rkt`, `fsh/pack.rkt`, `raco fsh <dir> --pack [out.tgz]`)
`compile-project` finds the files and sushi-config.yaml, parses each file (diagnostics name the file),
compiles with the dependency packages in the SD context, and never exits: parse errors and failing
entities are `fhir/diagnostic` values positioned at the entity. `pack-project!` writes a FHIR NPM
package — no SUSHI involved; `sushi-config.yaml` is read by `ig-config.rkt`'s line-oriented reader
(canonical, fhirVersion, dependencies, id/packageId, version, name, title, status, license,
description, publisher.name): package/package.json (dependencies + the core package), conformance
resources and `Usage: #definition` instances in package/, other instances in package/example/, a
`.index.json` per folder. The package feeds `raco fhir-server serve-ig <tgz> --capability <id>`, which
now resolves the package's own dependencies from fhir-spec. `tests/pack-test.rkt` checks mCODE's
package; the fhir repo's fhir-server/tests/fsh-ig-server-r4-test.rkt serves it: 0 unmet requirements, and **283/283**
of the examples we compile validate against the profiles we compile (as the published package does).

A project may hold `.map` files: `project.rkt` parses them through `fml/project` (dynamic-require —
fsh does not depend on fml; a missing fml is a diagnostic), their StructureMaps join the artifacts
(packed as conformance resources), and `check.rkt` runs `check-project-map` on each in the package
context the compile used: paths against the structures, the examples of the source structure
transformed and validated against the target. `compile.rkt` registers a document's `Logical:`
models and custom resources as types (fhir/sd/logical) before instances compile, so `InstanceOf: <a
logical model>` builds. The worked project: fhir-examples/examples/intake-maps.
`raco fsh check <dir> [--capability id]` validates every example against the project's profiles and
positions each problem in the FSH (`check.rkt`, on fhir-validation; `--capability` loads fhir-server on
demand; see the main repo's CLAUDE.md, "The spec-author's loop"). `project-sd-packages` records the
package context a compile used, so the checker validates in the same one.

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

## Fidelity to SUSHI (2026-11-05)
`tests/ig-harness.rkt` classifies every remaining difference (`all-diffs` → `diff-class`, printed
largest first with how many resources each class touches) — the work list. What that pass fixed, each
a SUSHI behaviour now matched: parameterized RuleSet arguments substituted verbatim with `\)` `\,`
unescaped; `* a and b MS` flags every path; `Reference (A or B)`; `Canonical(X)` lexemes resolved to
urls (profiles, value sets, code systems, logical models); `only` keeps the Parent's type extension;
`* path insert RS` and indented inserts apply under the path; inserts inside Invariants and RuleSets
(the grammar reads `* insert X` in a RuleSet as a code-insert with no codes); several `obeys`
accumulate; a logical model is named by its url (type code, Parent), `Characteristics:` → type-
characteristics extensions, Mapping by id-rooted ids; a slice with constrained children is in the
differential (bare, with sliceName) and carries copies of the sliced element's slice children;
`extension[<url|alias>]` means the Parent's slice typed with it; complex Extensions: typed
sub-extension slices get no url/extension scaffold, value[x] 0..0; `from VS` defaults to required.
Instances: `extension[USCoreRace]` by the extension's url, nested sub-extension urls from the
extension's definition (fixed url, else the typed extension's canonical), slices under any element
from the InstanceOf profile; `[slice][k]` contiguous blocks; only REQUIRED slices take profile order
up front; an index-less repeat is element 0 (a later assignment replaces); a code into a Quantity /
a coded quantity unit → system+code+unit; `system|version#code`; Instance `Title:`/`Description:`
fill title/description. The harness drops what the IG Publisher adds and SUSHI does not: a bare
`{id, path}` differential element (a root on some profiles — by processing order, see
PublisherBase.generateSnapshot), nested resources' narratives, narrative whitespace/quotes/`<x/>`.
A caret into an element's `binding` or `extension` EDITS what the Parent's element carries — a
base type's snapshot element included (`profile-elements` serves base types; `seed-caret-target`):
IPS's ImagingStudy keeps the base's display-hint ahead of its `^extension[obligation][+]` entries
and its procedureCode binding keeps the base's strength and value set, without the core package's
bindingName and explicit-type-name annotations (SUSHI drops those: Composition.section has its
obligations alone). A Logical whose Parent is a logical model of the document carries the parent's
type-characteristics before its own (IPSSectionsLM). The harness drops the Publisher's OID from an
R4 ConceptMap too (a single Identifier there).
Remaining (named in the CI output): mCODE examples whose published JSON differs from the tagged FSH
(eve-anyperson), a `{Base}` UCUM unit, a must-support type extension; IPS Bundle narratives and one
numeric-looking entry.

## The decompiler's oracle (`tests/gofsh-ips-test.rkt`)
The GoFSH direction measured on a real package: every resource of the PUBLISHED IPS 2.0.1 package
(32 StructureDefinitions, 36 ValueSets, the CapabilityStatement, the examples) is decompiled with
`fhir->fsh`, written as FSH (one file per resource type), compiled back, and held to the package by
the same harness (`check-fsh-ig`: pairs, exact, complete, differential elements, difference classes).
Current: **117/117 paired; 117 exact; 438/438 differential elements** — the whole package round
trips — ratcheted (the first run: 48 exact, 3 elements). What it drove in `decompile.rkt`: Parents without their version pin (the
compiler reads a pinned core type too, `strip-core-version`); `resourceType` first inside an
embedded resource, a lone `_x` written at `x`; value sets' `codes from system X [and valueset …]
where p op v and …` (`filter->node`; the printer prints filter lists); logical models' element
carets, `Characteristics:` from the type-characteristics extensions, a logical type by name;
`^short`/`^definition` and every other element property as carets, `only <profile canonical>`,
`^binding.description` and the binding's extensions, `^mustSupport = false`; a plain slice's own
properties at `path[slice]`, an extension slice as `contains <url> named <slice>`; exact cards (a
published package's integers read back inexact); root-element rules at `.`; a type slice as its
choice path (`performedDateTime 0..1`); SUSHI's derived copies (a slice child under every slice,
equal in content) left to the compiler; the snapshot, narrative, date, publisher, contact and
jurisdiction (the IG Publisher's) not written. A conformance resource decompiles as a `#definition`
instance (`definition-types`), an Invariant carries its `XPath:`, and a logical model's other
extensions are indexed after its characteristics as PUBLISHED (the compiler writes a local Parent's
again).

## The decompiler's oracle 2: mCODE (`tests/gofsh-mcode-test.rkt`)
The published mCODE 4.0.0 package (349 resources, with US Core and Genomics Reporting in context)
decompiled, recompiled and compared the same way. Current: **349 paired; 341 exact, 345 with all
published content; 472/473 differential elements** — ratcheted. The residue is named in the CI output
(a handful of examples and the one differential element the forward corpus also misses).

## Real-IG corpus 5: Genomics Reporting 2.0.0 (`tests/ig-genomics-test.rkt`)
HL7 Genomics Reporting's own FSH (`tests/vendor/genomics-reporting-2.0.0`, HL7/genomics-reporting at
commit c2cf861 — the commit whose resource ids match the published package 46/46; the 2.0.0 tag's do
not) against fhir-spec's `hl7.fhir.uv.genomics-reporting-2.0.0.tgz`. What it adds: OperationDefinitions
and ConceptMaps as instances, deep component slicing through RuleSets, large code systems, examples
authored as Bundles. Current: **0 compile errors; 141 paired; 121 exact, 129 with all published
content; 254/259 differential elements** — ratcheted. It drove: `drop-unchanged` (SUSHI writes what
changes — a property equal to the Parent's element's leaves the differential), `extension[<Extension
name>]` resolved to the slice typed with it in the profile's OWN `contains` (`extension contains
GenomicReportNote named coded-note`, then `extension[GenomicReportNote] ^requirements`), quoted
concept codes (`$PHARMVAR#"CYP2C9 *2/*5"`) unquoted, the harness dropping the Publisher's
`structuredefinition-category`/`-security-category` extensions. Named residue, not chased: the
package was built by SUSHI 2.x, which wrote a type-specific choice path as an element of its own
(`Observation.valueQuantity`) where SUSHI 3 and we write the type slice (`value[x]:valueQuantity`,
5 differential elements); its OperationDefinitions and several examples drifted after publication
(parameter names, types, cardinalities; coding versions the FSH never writes; the examples it kept
in XML/JSON).

## SUSHI's importer tests as an oracle (`tests/sushi-importer-test.rkt`, `fsh/lint.rkt`)
SUSHI's own importer test suite (`tests/vendor/sushi-importer/cases.json`, 515 FSH texts extracted
from SUSHI 3.21.0's `FSHImporter*.test.ts` by `extract.py` — Apache-2.0; 111 of them with the error
or warning SUSHI logs, file and line) as a parse-and-diagnostics corpus: a text SUSHI rejects with an
error must produce an error here, on the same line when SUSHI names one; a text SUSHI accepts must
parse. Our side is `parse-fsh-doc/diagnostics` (parse.rkt): every entity's parse error (the other
entities still parse) plus the import-level checks SUSHI makes after parsing, in `lint.rkt` — a
second entity of a kind with one name, an alias redefined to another value, a metadata keyword
declared twice, an Instance without InstanceOf or with an unknown Usage, a cardinality with neither
side, an added element missing its min, max or short (the grammar makes the short optional, as
SUSHI's recovery does, so the lint can say which is missing), a choice of references or canonicals as
a value, a concept with a system in a CodeSystem, several concepts before a caret or insert rule in
a ValueSet, filter values of the wrong kind (`true`/`false` stay `(bool …)` in the datum so they are
told from strings), and `=`/`->` without their spaces. The parser enforces SUSHI's indentation rules
as positioned errors (two spaces per level, the first rule left-aligned, at most one level deeper
than the rule above, nothing indented under a path-less rule, `.` only at the top level).
`compile-project` parses through the same entry point, so a project reports every file's findings.
Current: **514/515 — 408 agree, 95 of them on SUSHI's own line, 11 where SUSHI logs an error its test leaves unasserted; 0 missed errors, 0 over-reports, 0 crashes; the one line elsewhere is the empty RuleSet, whose error SUSHI positions at the next entity** — ratcheted (agreements up, over-reports and crashes down). The
over-reports are read against SUSHI's source: a shape-asserting test (no message assertion) can
hide an error SUSHI did log — `* address 1..* Address` without a short does (BadDefs.fsh asserts
that very error). Remaining misses are named in the CI output.

## Raw SUSHI as an oracle (SUSHI 3.21.0, 2026-11-06)
The published packages are SUSHI output after the IG Publisher has run, so matching them can hide
what SUSHI itself writes. Running SUSHI 3.21.0 on the vendored IGs, the golden `.fsh` files and a
probe project showed what it really does, and fsh now does the same:
- `sushi-config.yaml` is read as YAML (`yaml.rkt`: block and flow collections, quoted and block
  scalars, comments; anchors, tags and multi-document streams are errors). `ig-config` keeps the
  whole tree for what the ImplementationGuide will need, and `fhirVersion` may be a list.
- `status` comes from the config (`fsh-config-status`) unless `^status` sets it. `version` is
  written only when the config says `FSHOnly: true` (`fsh-config-version`); otherwise the IG
  Publisher sets it. There is no more hard-coded `0.1.0`. The golden test runs with version 0.1.0,
  as the config SUSHI built it with.
- No `mapping` is written on a profile; the IG Publisher adds the base's, so the harness drops it
  as Publisher-added. A `Mapping:` entity writes only its own declaration.
- `Context:` compiles: a quoted item is `fhirpath`; an unquoted one is `extension` with the
  Extension's canonical when it names one (local or a dependency's), else `element`.
- The type-characteristics `can-bind` extension goes on R5+ Extensions only, not R4 ones.
- A root `^extension[Name]` (an alias without `$`, or an extension's name or id) resolves to the
  Extension's canonical. Before this, mCODE's 89 `^extension[FMM]` carets wrote the url "FMM";
  the harness's FMM normalisation hid it.
Measured against raw SUSHI (byte-identical apart from text/snapshot): IPS 102/118, mCODE 326/350,
golden 19/22 (the remaining diffs are differential details). The SUSHI outputs are not vendored yet.

## Editor and agent support (`fsh/support.rkt`)
What FSH offers the family's hubs through `fhir/lang/support` (the contract in the foundation): the
LSP server (fhir-lsp) loads `fsh/support` by its collection name. It
gives a document's diagnostics with its project (`compile-project #:texts`), the project's files, the
outline, hover (an element path in the base type's snapshot; a name's declaration), go to definition
across the project, completion (`support/complete.rkt`: keywords, Parent/InstanceOf types and
profiles, value sets after `from`, rule sets, invariants, the base type's elements at a path, flags).
`support/symbols.rkt` reads declarations,
aliases, rule paths and Parents from the TEXT (parser-free, so it works while a document does not
parse yet). The hubs know no FSH: a language change and its editor support live and test together here.

## Workspace support (`workspace.rkt`)
The workspace's `.fsh` files are one project: `check-fsh-project` with the workspace's canonical,
FHIR version and packages (`#:canonical`/`#:fhir-version`/`#:packages`, new on check-fsh-project and
compile-project — the packages go after a sushi-config's dependencies). `run` of a file = the
resources its entities compile to, each with the problems inside its span; `explain` of an entity =
its resource and problems (examples' validation issues traced to rules). Fixed with it: check.rkt's
fallback `ig-config` had 11 fields after the struct grew a 12th, so checking a project without a
sushi-config.yaml raised.

## Documentation for users and agents (2026-11-09; the family's pattern, cql first)
`guide.md` is the language for agents — official references, a cheat sheet, a worked example,
running it (command line, module, workspace), common problems with the messages they produce —
provided by `fsh/workspace` as `guide` (fhir-mcp's `learn://fsh`, replacing the hub's own
`learn/fsh.md`). `tests/guide-test.rkt` checks every ```fsh block as a project (no error, at least
one example validated) and that each quoted message is what the checker reports. The hub's old
cheat sheet used `#` comments (a parse error in FSH) and its example broke its own profile, which is
why the claims are tested. `.claude/skills/fsh/SKILL.md` is the workflow without MCP (every command
in it was run). `scribblings/fsh.scrbl` is the manual; `tests/docs-test.rkt` renders it and fails on
a broken reference to its sections or `fsh/…` bindings.
Writing them turned up silent mistakes, each now an error (SUSHI reports them too):
- a `Parent` nothing defines (not a FHIR type, nor a local profile/extension/logical model, nor in
  the packages) — `sd.rkt`; the profile used to compile with an invented base. Skipped when no SD
  index is available (undecidable).
- two definitions of one kind sharing an id (StructureDefinitions — Profile/Extension/Logical/
  Resource — ValueSets, CodeSystems; the id is `Id:` else the name) — `project.rkt`
  `duplicate-id-diagnostics`, at the later entity, naming the first.
- an `insert` of a RuleSet the project does not define — `compile.rkt` `unknown-ruleset!`, a rule
  diagnostic at the insert (it used to be left unexpanded).
- `check --capability` without a sushi-config.yaml now says so instead of failing to pack.
Not fixed here (fhir-validation): a `valueString` in a slice narrowed to `value[x] only Quantity`
is reported neither by `fsh check` nor by `raco fhir-validate`; the manual's old quick start relied
on it.
