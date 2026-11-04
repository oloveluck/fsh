#lang scribble/manual
@(require (for-label racket/base racket/contract
                     fsh/parser/parse fsh/compile fsh/decompile fsh/print fsh/project fsh/pack fsh/check fsh/ig-config))

@title{fsh: FHIR Shorthand for Racket}

A @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FHIR Shorthand} compiler and its reverse. FSH to
FHIR: profiles, extensions, logical models and resources, instances, value sets, code systems, rule
sets, mappings, slicing and reslicing, for R4 or R5, with a sushi-config.yaml's canonical, version
and dependencies. FHIR to FSH: GoFSH-style decompilation. @hash-lang[] @tt{fsh} makes a @tt{.fsh}
file a module. @tt{raco fsh check} validates a project's examples against its own profiles and
positions every problem in the FSH.

Measured on real IGs against their published packages: IPS 2.0.1 and mCODE 4.0.0 compile from their
own sources with no errors (mCODE's 283 examples all validate against the profiles compiled here, as
they do against the published package); the remaining differences are listed in the repository's
notes.

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir},
@tt{fhirpath} and @tt{fhir-validation}. Real projects also need @tt{fhir-spec} (the core and
dependency packages).

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhir/fhir-spec fhirpath vcl fhir/fhir-validation fsh   # from source
}

@section{@hash-lang[] @tt{fsh}}
@defmodulelang[fsh]

The module provides @racket[document] (the entity ASTs), @racket[compiled] (the FHIR resources as
jsexprs) and @racket[resources] (the same as typed nodes, read leniently). The target is R5. A parse
error is a compile error positioned in the file. Running the file prints the JSON, one resource per
line.

@verbatim|{#lang fsh
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

Instance: ann
InstanceOf: Patient
* name.family = "Ng"
* gender = #female}|

@section{Compiling}
@defmodule[fsh/compile]

@defproc[(fsh->fhir [src string?]) (listof hash?)]{FSH source to its FHIR resources.}
@defproc[(compile-doc [entities (listof struct?)]) (listof hash?)]{
A parsed document: aliases resolved, rule sets expanded, each entity compiled. Raises on the first
entity that fails.}
@defproc[(compile-doc/errors [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)))]{
Like SUSHI, a failing entity is reported (as @racket[entity-label] and the exception) and the rest
of the document still compiles.}
@defproc[(compile-doc/diagnostics [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)) list?)]{
Also every rule-level diagnostic (@tt{fhir/diagnostic}, positioned at the rule): an element path
whose first step the base type does not have, for one. Compilation continues past them.}
@defproc[(compile-entity [e struct?] [aliases hash? (hash)] [profiles hash? (hash)] [invariants hash? (hash)]) (or/c hash? #f)]{One entity.}
@defproc[(entity-label [e struct?]) string?]{@racket["profile: Name"].}
@defproc[(rule-path-of [rule any/c]) (or/c string? #f)]{The element path a profile rule constrains.}
@defparam[fsh-fhir-version version string?]{The FHIR version stamped on StructureDefinitions, @racket["5.0.0"] by default. Set @racket["4.0.1"] and register the R4 tower for an R4 IG.}
@defparam[canonical-base url string?]{The canonical base, @racket["http://example.org"] by default.}
@defparam[emit-snapshot? on? boolean?]{Also generate snapshots (@tt{fhir/sd/snapshot}; needs the core package). Off by default.}

@subsection[#:style 'unnumbered]{@tt{fsh/parser/parse}}
@defmodule[fsh/parser/parse]

@defproc[(parse-fsh-doc [src string?] [#:source source (or/c string? path? #f) #f]) list?]{
A document's entity ASTs. A failure raises @racket[exn:fail:diagnostic] positioned in @racket[source]:
@tt{t.fsh:3:18: fsh parse error: unexpected caret sequence `^short`}.}
@defproc[(parse-fsh-doc/located [src string?] [#:source source (or/c string? path? #f) #f]) (listof pair?)]{Each entity paired with the location of its first token.}
@defproc[(parse-fsh-entity [src string?]) any/c]{One entity.}

@subsection[#:style 'unnumbered]{@tt{fsh/print}}
@defmodule[fsh/print]

@deftogether[(
@defproc[(print-doc [entities list?]) string?]
@defproc[(print-entity [e any/c]) string?]
@defproc[(print-value [v any/c]) string?])]{AST back to FSH text.}

@section{Decompiling}
@defmodule[fsh/decompile]

@defproc[(fhir->fsh [resource hash?] ...) string?]{
FHIR resources as an FSH document: idiomatic rules where the shape is recognised (cardinality, flags,
bindings, @tt{only}, @tt{obeys}, concepts, includes, slicing) and a lossless @tt{^caret} for the
rest. Invariants are re-synthesised from the SD constraints. Reads element types from the registered
tower.}
@defproc[(node->fsh [node fhir?] ...) string?]{The same from typed nodes.}
@defproc[(fhir->fsh-entity [resource hash?]) any/c]{One resource as an entity AST.}

@section{Projects}
@defmodule[fsh/project]

@defstruct*[project ([config (or/c ig-config? #f)] [fhir-version string?] [files (listof path?)] [texts hash?]
                     [located list?] [artifacts (listof hash?)] [diagnostics list?] [missing list?]
                     [sd-packages (listof path?)])]{
A compiled project: its sushi-config, the source texts by file, the entities with their locations,
the resources, every diagnostic (parse errors per file; a failing entity positioned at its
declaration), the dependencies that were not found, and the packages the compile resolved against
(dependencies, then core), which @racket[check-fsh-project] validates in.}
@defproc[(compile-project [src path-string?]
                          [#:canonical canonical (or/c string? #f) #f]
                          [#:fhir-version fhir-version (or/c string? #f) #f]
                          [#:snapshot? snapshot? boolean? #f]) project?]{
A file or a directory. The arguments override sushi-config.yaml; a version starting with 4 registers
the R4 tower. Never exits: everything wrong is in @racket[project-diagnostics].}
@defproc[(pack-project! [p project?] [out path-string?]) path?]{The FHIR NPM package (needs the config).}
@defproc[(find-config [src path-string?]) (or/c path? #f)]{sushi-config.yaml beside the sources or up to two levels up.}
@defproc[(fsh-files [src path-string?]) (listof path?)]{A file, or every @tt{.fsh} under a directory.}

@subsection[#:style 'unnumbered]{@tt{fsh/ig-config}}
@defmodule[fsh/ig-config]

@defstruct*[ig-config ([canonical any/c] [fhir-version any/c] [dependencies list?] [id any/c] [name any/c] [title any/c]
                       [version any/c] [status any/c] [license any/c] [description any/c] [publisher any/c])]{
What compiling and packaging read from sushi-config.yaml; any field may be @racket[#f]. A dependency
is @racket[(cons package-id version)].}
@defproc[(read-ig-config [file path-string?]) ig-config?]
@defproc[(dependency-packages [deps (listof (cons/c string? string?))]) (values (listof path?) (listof (cons/c string? string?)))]{
Each dependency's package found in @tt{fhir-spec} as @tt{<id>-<version>.tgz}, and the ones missing.}

@subsection[#:style 'unnumbered]{@tt{fsh/pack}}
@defmodule[fsh/pack]

@defproc[(pack-package! [out path-string?] [config ig-config?] [resources (listof hash?)] [usages (hash/c string? string?)]) path?]{
Writes the package: @tt{package.json} from the config, conformance resources and @tt{#definition}
instances in @tt{package/}, other instances in @tt{package/example/}, an index per folder.
@racket[usages] maps an instance id to @racket["#example"] or @racket["#definition"].}
@defproc[(conformance-type? [type string?]) boolean?]

@section{Checking}
@defmodule[fsh/check]

@defproc[(check-fsh-project [dir path-string?] [#:capability capability (or/c string? #f) #f]) check-report?]{
The project end to end: compile; validate every example against the project's profiles in the same
package context (the project's own artifacts ahead of its dependencies and core); with a
CapabilityStatement id, the generated server's unmet requirements (loads @tt{fhir-server} on demand).
Every problem is a @tt{fhir/diagnostic} positioned in the FSH: a validation issue is traced to the
instance rule that wrote the element and names the profile rule it breaks.}
@defstruct*[check-report ([diagnostics list?] [texts hash?] [examples exact-nonnegative-integer?]
                          [invalid exact-nonnegative-integer?] [artifacts list?])]
@defproc[(trace-rule [rules list?] [issue-path string?]) (or/c any/c #f)]{
The rule whose path best matches a validator path: the type root dropped, brackets and choice suffixes
normalised, the longest leading run of steps wins.}

@section{Command line}

@verbatim{
raco fsh <file.fsh | dir> [-o <outdir>] [--snapshot] [--canonical <url>] [--fhir-version 4.0.1|5.0.0] [--pack [out.tgz]]
raco fsh --decompile <file.json>...
raco fsh check <dir> [--capability <CapabilityStatement id>]
}

Compiles one file or every @tt{.fsh} under a directory to @tt{<resourceType>-<id>.json} files in
@tt{<outdir>} (default @tt{fsh-generated/}). A sushi-config.yaml supplies the canonical, version and
dependencies; flags override it. @tt{--pack} also writes the FHIR NPM package, which
@tt{raco fhir-server serve-ig} loads. @tt{check} prints each problem with its source line and a
caret, then @tt{fsh check: N examples, M with errors; E error(s), W warning(s)}, and exits 1 on an
error. @tt{--format text|json|outcome} on any command. Exit codes: 0, 1 on an FSH error, 2 on usage.
