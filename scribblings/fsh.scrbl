#lang scribble/manual
@(require (for-label racket/base racket/contract
                     fsh/parser/parse fsh/lint fsh/compile fsh/decompile fsh/print fsh/project fsh/pack fsh/check fsh/ig-config
                     (only-in fhir/diagnostic diagnostic?)))

@title{fsh: FHIR Shorthand}

A @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FHIR Shorthand} compiler. You write profiles,
extensions, value sets, code systems and example instances in @tt{.fsh} files, the way you would for
SUSHI; @tt{raco fsh check} compiles them and validates every example against the profiles, pointing
at the FSH line behind each problem. It also goes the other way (FHIR JSON to FSH), and a @tt{.fsh}
file is a Racket module whose resources a program can use.

Held to real guides: IPS 2.0.1, mCODE 4.0.0, Da Vinci Plan-Net 1.2.0 and Da Vinci HRex 1.1.0 compile
from their own sources with no errors, their published packages are reproduced resource for resource but for a
handful of named differences, and mCODE's 283 examples validate against the profiles compiled here,
as they do against the published package. Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family.

@table-of-contents[]

@section{Quick start}

@bold{1. Install.} The packages are not on the Racket package server yet; clone the repositories
side by side and install from source:

@verbatim{
raco pkg install --link --auto fhir/fhir fhir/fhir-spec fhirpath vcl fhir/fhir-tx fhir/fhir-validation fsh
}

@bold{2. Write a project.} A directory with a @tt{sushi-config.yaml} and @tt{.fsh} files under
@tt{input/fsh/} (the SUSHI layout):

@verbatim|{# ig/sushi-config.yaml
id: example.bp
canonical: http://example.org/bp
fhirVersion: 4.0.1
dependencies:
  hl7.fhir.us.core: 6.1.0}|

@verbatim|{// ig/input/fsh/bp.fsh
Alias: $loinc = http://loinc.org

Profile: ScreeningBloodPressure
Parent: Observation
Id: screening-blood-pressure
* status = #final
* code = $loinc#85354-9 "Blood pressure panel"
* subject 1..1
* component ^slicing.discriminator.type = #pattern
* component ^slicing.discriminator.path = "code"
* component ^slicing.rules = #open
* component contains systolic 1..1 and diastolic 1..1
* component[systolic].code = $loinc#8480-6 "Systolic blood pressure"
* component[systolic].value[x] only Quantity

Instance: reading-1
InstanceOf: ScreeningBloodPressure
Usage: #example
* status = #final
* subject = Reference(Patient/ann)
* component[systolic].valueString = "high"}|

@bold{3. Check it.}

@verbatim{
$ raco fsh check ig/
ig/input/fsh/bp.fsh:22:3: fsh validate error: instance reading-1: Observation.component.value[x]:
    valueString is not allowed in slice 'systolic' (value[x]: Quantity)
    (constrained by profile ScreeningBloodPressure at ig/input/fsh/bp.fsh:15:3)
22 | * component[systolic].valueString = "high"
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fsh check: 1 examples, 1 with errors; 1 error(s), 0 warning(s)
}

The error sits on the instance rule that wrote the bad element and names the profile rule it breaks.
Fix the line (@tt{valueQuantity = 140 'mm[Hg]'}), run again, exit 0. Then @tt{raco fsh ig/} writes
the resources to @tt{ig/fsh-generated/}, and @tt{raco fsh ig/ --pack} writes the FHIR NPM package.

@section{Writing FSH}

The language is SUSHI's; the reference is the @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FSH
specification}. In brief: an entity (@tt{Profile}, @tt{Extension}, @tt{Instance}, @tt{ValueSet},
@tt{CodeSystem}, @tt{Invariant}, @tt{RuleSet}, @tt{Mapping}, @tt{Logical}, @tt{Resource},
@tt{Alias}) is its keyword lines followed by rules, one per @tt{*}:

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{rule} @bold{example})
      (list "cardinality and flags" @tt{* subject 1..1 MS})
      (list "assignment (a pattern; add (exactly) for a fixed value)" @tt{* code = $loinc#85354-9 "Blood pressure panel"})
      (list "binding" @tt{* category from ObservationCategoryCodes (required)})
      (list "type" @tt{* value[x] only Quantity or CodeableConcept})
      (list "slicing" @tt{* component contains systolic 1..1 and diastolic 1..1})
      (list "invariant" @tt{* obeys bp-1})
      (list "rule set" @tt{* insert CommonMetadata})
      (list "anything else, through the element definition" @tt{* code ^short = "What was measured"}))]

Everything SUSHI accepts for R4 and R5 is accepted here: indented rules (path context), parameterized
rule sets, reslicing, extension scaffolding, logical models and custom resources, mappings, and the
dependencies named in @tt{sushi-config.yaml}, which are found in @tt{fhir-spec} as
@tt{<id>-<version>.tgz}.

@section{Using it}

@subsection{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco fsh check ig/} "compile; validate every example against the project's profiles; problems on their FSH lines")
      (list @tt{raco fsh check ig/ --capability <id>} "also what a server generated from that CapabilityStatement would not do (needs fhir-server)")
      (list @tt{raco fsh ig/} @elem{compile every @tt{.fsh} to @tt{ig/fsh-generated/<Type>-<id>.json}})
      (list @tt{raco fsh ig/ --pack [out.tgz]} "also write the FHIR NPM package")
      (list @tt{raco fsh file.fsh} "one file, with the default canonical and R5")
      (list @tt{raco fsh --decompile sd.json} "FHIR to FSH"))]

Flags: @litchar{-o <dir>}, @litchar{--fhir-version 4.0.1|5.0.0}, @litchar{--canonical <url>},
@litchar{--snapshot} (also generate snapshots), @litchar{--format text|json|outcome} (how problems
are reported: text with a caret on stderr, or JSON or a FHIR OperationOutcome on stdout for a
program). Exit codes: 0, 1 on an FSH error, 2 on usage.

A package written by @litchar{--pack} is served by @tt{raco fhir-server serve-ig out.tgz --capability
<id>} (the fhir-server package), with its dependencies resolved from @tt{fhir-spec}.

@subsection{As a module}

@racket[(require "bp.fsh")] compiles the file when the program compiles; a parse or compile error
is positioned in it. The module provides @racket[compiled] (the FHIR resources as jsexprs),
@racket[resources] (typed nodes) and @racket[document] (the entity ASTs). @tt{racket bp.fsh} prints
the JSON. A single file is compiled for R5 with the default canonical; use @racket[compile-project]
for a project with a config.

@racketblock[
(require (only-in "bp.fsh" [compiled clinic])
         fhir/data-source (only-in fhir/conformance/operations ensure-valid))
(code:comment "the profile is resolvable by url, because requiring the module registered it")
(parameterize ([current-data-source (resources->data-source clinic)])
  (ensure-valid reading #:profile "http://example.org/StructureDefinition/screening-blood-pressure"))
]

@subsection{From Racket}

@racketblock[
(require fsh/compile fsh/project fsh/check fsh/decompile)
(fsh->fhir text)                                   (code:comment "source -> (listof jsexpr)")
(define p (compile-project "ig/"))                 (code:comment "with its sushi-config.yaml")
(project-artifacts p)                              (code:comment "the resources")
(project-diagnostics p)                            (code:comment "every problem, positioned")
(check-report-diagnostics (check-fsh-project "ig/"))
(fhir->fsh sd)                                     (code:comment "jsexpr -> FSH text")
]

@subsection{Maps in a project}

A project may also hold @tt{.map} files in the FHIR Mapping Language (the @tt{fml} package, loaded
when present). Their StructureMaps join the package, and @tt{check} checks each map against the
project's structures and transforms the examples of its source structure, validating the results
against the target; findings are positioned in the map. A @tt{Logical:} model is registered as a
type, so an @tt{Instance:} of it builds like any resource. The worked project is
@tt{fhir-examples/examples/intake-maps}; the fml document walks through it.

@section{In an editor, for an agent}

@tt{fsh/support} is what the family's language server (fhir-lsp) loads for
FSH: a document's diagnostics with its project, the project's files, the outline, hover (an element
path in the base type; a name's declaration), go to definition across the project, completion
(keywords, Parent and InstanceOf types and profiles, value sets after @tt{from}, rule sets,
invariants, the base type's elements at a path, flags). It is a @tt{fhir/lang/support} value.
@tt{fsh/workspace} is what fhir-mcp runs for an agent: the workspace's FSH as one project, every
example validated, each problem positioned in the FSH.

@section{Limits}

Measured against the IGs' published packages: IPS 2.0.1 pairs 114 resources, 94 identical, 108 with
all published content, all 438 differential elements; mCODE 4.0.0 pairs 349, 227 identical, 339
complete, 472 of 473 differential elements; Plan-Net 1.2.0 (its package and US Core 3.1.1 fetched from
the registry when the tests run) pairs 109, 78 identical, 99 complete, all 321 differential
elements; HRex 1.1.0 (three US Core versions in context) pairs 46, 31 identical, all 46 complete, all
138 differential elements; Genomics Reporting 2.0.0 pairs 141, 113 identical, 129 complete, 254 of 259
differential elements. The decompiler is measured the same way: the published IPS and mCODE packages
decompiled to FSH, compiled back and compared (@filepath{tests/gofsh-ips-test.rkt},
@filepath{tests/gofsh-mcode-test.rkt}) — all 117 IPS resources come back identical, 341 of 349 mCODE
resources. The parser and the linter are measured against SUSHI's own importer test suite
(@filepath{tests/sushi-importer-test.rkt}: 515 texts; 514 verdicts agree, no missed error, no over-report). The tests print every
remaining difference, classified, largest first. The differences:

@itemlist[
@item{A few caret extensions on one IPS profile land in a different order (a url-keyed extension
among @tt{[+]}-indexed ones); a few mCODE examples whose published JSON differs from the tagged FSH
sources; one element's @tt{min}; a UCUM unit with an annotation; in Plan-Net, instance details the
test names. In Genomics Reporting, what the published package carries that its FSH does not: a
SUSHI 2 representation of type-specific choice paths (@tt{Observation.valueQuantity} as an element
of its own), OperationDefinitions and examples that drifted after publication, coding versions the
FSH never writes.}
@item{No ImplementationGuide resource, no narratives, no ActorDefinition conversion: these are
publisher output.}
@item{@tt{Characteristics:} and @tt{Context:} metadata are parsed but not read (an extension's
context comes from a @tt{^context} caret). An @tt{insert} of an unknown RuleSet is left unexpanded,
not reported.}
@item{Snapshots are opt-in (@litchar{--snapshot}, @racket[emit-snapshot?]); the default output is
the differential. R4 and R5 only.}
@item{No code validation: a binding records its strength and value set, and @tt{check} runs without
a terminology service, so only bindings whose value set enumerates its codes are checked.}
@item{The decompiler gives idiomatic FSH for StructureDefinitions, ValueSets and CodeSystems; any
other resource becomes a per-leaf @tt{Instance}, and nothing is extracted into Aliases or RuleSets.}]

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fsh}}
@defmodulelang[fsh]

Provides @racket[document], @racket[compiled] and @racket[resources], as above.

@subsection[#:style 'unnumbered]{@tt{fsh/compile}}
@defmodule[fsh/compile]

@defproc[(fsh->fhir [src string?]) (listof hash?)]{FSH source to its FHIR resources.}
@defproc[(compile-doc [entities (listof struct?)]) (listof hash?)]{
A parsed document: aliases resolved, rule sets expanded, each entity compiled. Raises on the first
entity that fails.}
@defproc[(compile-doc/errors [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)))]{
A failing entity is reported (as @racket[entity-label] and the exception) and the rest of the
document still compiles.}
@defproc[(compile-doc/diagnostics [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)) list?)]{
Also every rule-level diagnostic (@tt{fhir/diagnostic}, positioned at the rule), such as an element
path whose first step the base type does not have. Compilation continues past them.}
@defproc[(compile-entity [e struct?] [aliases hash? (hash)] [profiles hash? (hash)] [invariants hash? (hash)]) (or/c hash? #f)]
@defproc[(entity-label [e struct?]) string?]{@racket["profile: Name"].}
@defproc[(rule-path-of [rule any/c]) (or/c string? #f)]{The element path a profile rule constrains.}
@defparam[fsh-fhir-version version string?]{The FHIR version stamped on StructureDefinitions, @racket["5.0.0"] by default; @racket["4.0.1"] with the R4 tower registered compiles an R4 IG.}
@defparam[canonical-base url string?]{@racket["http://example.org"] by default.}
@defparam[emit-snapshot? on? boolean?]{Also generate snapshots (needs the core package). Off by default.}

@subsection[#:style 'unnumbered]{@tt{fsh/parser/parse}}
@defmodule[fsh/parser/parse]

@defproc[(parse-fsh-doc [src string?] [#:source source (or/c string? path? #f) #f]) list?]{
A document's entity ASTs. A failure raises @racket[exn:fail:diagnostic] positioned in @racket[source]:
@tt{t.fsh:3:18: fsh parse error: unexpected caret sequence `^short`}.}
@defproc[(parse-fsh-doc/located [src string?] [#:source source (or/c string? path? #f) #f]) (listof pair?)]{Each entity paired with the location of its first token.}
@defproc[(parse-fsh-doc/diagnostics [src string?] [#:source source (or/c string? path? #f) #f])
         (values (listof pair?) (listof diagnostic?))]{
Every entity that parses (each with its location) and every diagnostic: an entity's parse error leaves
the other entities parsing, and the document is then checked as SUSHI's importer checks it
(@racketmodname[fsh/lint]: duplicate names and metadata, an alias redefined, an Instance without
@tt{InstanceOf}, a cardinality with neither side, an added element missing its min, max or short, a
choice of references as a value, filter values of the wrong kind, operators without their spaces).
Indentation that SUSHI rejects is a parse error at the rule. Projects parse through this.}
@defproc[(parse-fsh-entity [src string?]) any/c]{One entity.}

@subsection[#:style 'unnumbered]{@tt{fsh/lint}}
@defmodule[fsh/lint]
@defproc[(lint-document [items (listof (list/c any/c any/c list?))]) (listof diagnostic?)]{
The import-level checks over a parsed document: each item is an entity, its location and its tokens
(as @racket[parse-fsh-doc/diagnostics] builds them). Every finding is an error positioned where it is
written.}

@subsection[#:style 'unnumbered]{@tt{fsh/print}}
@defmodule[fsh/print]

@deftogether[(
@defproc[(print-doc [entities list?]) string?]
@defproc[(print-entity [e any/c]) string?]
@defproc[(print-value [v any/c]) string?])]{AST back to FSH text.}

@subsection[#:style 'unnumbered]{@tt{fsh/decompile}}
@defmodule[fsh/decompile]

@defproc[(fhir->fsh [resource hash?] ...) string?]{
FHIR resources as an FSH document: idiomatic rules where the shape is recognised (cardinality, flags,
bindings, @tt{only}, @tt{obeys}, concepts, includes, slicing) and a lossless @tt{^caret} for the
rest; invariants re-synthesised from the SD constraints. Reads element types from the registered
tower.}
@defproc[(node->fsh [node fhir?] ...) string?]{The same from typed nodes.}
@defproc[(fhir->fsh-entity [resource hash?]) any/c]{One resource as an entity AST.}

@subsection[#:style 'unnumbered]{@tt{fsh/project}}
@defmodule[fsh/project]

@defstruct*[project ([config (or/c ig-config? #f)] [fhir-version string?] [files (listof path?)] [texts hash?]
                     [located list?] [artifacts (listof hash?)] [diagnostics list?] [missing list?]
                     [sd-packages (listof path?)] [maps list?])]{
A compiled project: its sushi-config, the source texts by file, the entities with their locations,
the resources (the maps' StructureMaps among them), every diagnostic (parse errors per file; a
failing entity positioned at its declaration), the dependencies not found, the packages the compile
resolved against (dependencies, then core), which @racket[check-fsh-project] validates in, and the
maps as @racket[(cons file fml-map)].}
@defproc[(compile-project [src path-string?]
                          [#:canonical canonical (or/c string? #f) #f]
                          [#:fhir-version fhir-version (or/c string? #f) #f]
                          [#:snapshot? snapshot? boolean? #f]) project?]{
A file or a directory. The arguments override sushi-config.yaml; a version starting with 4 registers
the R4 tower. Never exits: everything wrong is in @racket[project-diagnostics].}
@defproc[(pack-project! [p project?] [out path-string?]) path?]{The FHIR NPM package (needs the config).}
@defproc[(find-config [src path-string?]) (or/c path? #f)]{sushi-config.yaml beside the sources or up to two levels up.}
@defproc[(fsh-files [src path-string?]) (listof path?)]{A file, or every @tt{.fsh} under a directory.}
@defproc[(map-files [src path-string?]) (listof path?)]{Every @tt{.map} or @tt{.fml} under a directory.}

@subsection[#:style 'unnumbered]{@tt{fsh/ig-config}}
@defmodule[fsh/ig-config]

@defstruct*[ig-config ([canonical any/c] [fhir-version any/c] [dependencies list?] [id any/c] [name any/c] [title any/c]
                       [version any/c] [status any/c] [license any/c] [description any/c] [publisher any/c])]{
What compiling and packaging read from sushi-config.yaml; any field may be @racket[#f]. A dependency
is @racket[(cons package-id version)].}
@defproc[(read-ig-config [file path-string?]) ig-config?]
@defproc[(dependency-packages [deps (listof (cons/c string? string?))]) (values (listof path?) (listof (cons/c string? string?)))]{
Each dependency's package in @tt{fhir-spec} as @tt{<id>-<version>.tgz}, and the ones missing.}

@subsection[#:style 'unnumbered]{@tt{fsh/pack}}
@defmodule[fsh/pack]

@defproc[(pack-package! [out path-string?] [config ig-config?] [resources (listof hash?)] [usages (hash/c string? string?)]) path?]{
Writes the package: @tt{package.json} from the config, conformance resources and @tt{#definition}
instances in @tt{package/}, other instances in @tt{package/example/}, an index per folder.
@racket[usages] maps an instance id to @racket["#example"] or @racket["#definition"].}
@defproc[(conformance-type? [type string?]) boolean?]

@subsection[#:style 'unnumbered]{@tt{fsh/check}}
@defmodule[fsh/check]

@defproc[(check-fsh-project [dir path-string?] [#:capability capability (or/c string? #f) #f]) check-report?]{
The project end to end: compile; validate every example against the project's profiles in the same
package context (the project's own artifacts ahead of its dependencies and core); with a
CapabilityStatement id, the generated server's unmet requirements (loads @tt{fhir-server} on demand).
Every problem is a @tt{fhir/diagnostic} positioned in the FSH: a validation issue is traced to the
instance rule that wrote the element and names the profile rule it breaks. The project's maps are
checked against its structures in the same context (@tt{fml/project}, on demand), their findings
positioned in the map.}
@defstruct*[check-report ([diagnostics list?] [texts hash?] [examples exact-nonnegative-integer?]
                          [invalid exact-nonnegative-integer?] [artifacts list?])]
@defproc[(trace-rule [rules list?] [issue-path string?]) (or/c any/c #f)]{
The rule whose path best matches a validator path: the type root dropped, brackets and choice suffixes
normalised, the longest leading run of steps wins.}
