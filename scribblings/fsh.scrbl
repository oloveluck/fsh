#lang scribble/manual
@(require (for-label racket/base racket/contract
                     fsh/parser/parse fsh/compile fsh/decompile fsh/print fsh/project fsh/pack fsh/check fsh/ig-config))

@title{fsh: FHIR Shorthand for Racket}

A @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FHIR Shorthand} compiler and its reverse. FSH to
FHIR: profiles, extensions, logical models and resources, instances, value sets, code systems, rule
sets, mappings, slicing and reslicing, for R4 or R5, with a sushi-config.yaml's canonical, version
and dependencies. FHIR to FSH: GoFSH-style decompilation. A @tt{.fsh} file is a module, and
@tt{raco fsh check} validates a project's examples against its own profiles and positions every
problem in the FSH.

Measured on real IGs against their published packages: IPS 2.0.1 and mCODE 4.0.0 compile from their
own sources with no errors, and mCODE's 283 examples all validate against the profiles compiled
here, as they do against the published package; the remaining differences are listed in the
repository's notes.

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir},
@tt{fhirpath} and @tt{fhir-validation}. Real projects need @tt{fhir-spec} too (the core and
dependency packages).

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhir/fhir-spec fhirpath vcl fhir/fhir-validation fsh
    # from source
}

@section{Writing FSH}

Entities (@tt{Profile}, @tt{Extension}, @tt{Instance}, @tt{ValueSet}, @tt{CodeSystem},
@tt{Invariant}, @tt{RuleSet}, @tt{Mapping}, @tt{Logical}, @tt{Resource}, @tt{Alias}) with their
keyword lines, then rules, one per @tt{*}: cardinality (@tt{* subject 1..1}), flags (@tt{MS}),
assignments (@tt{* status = #final}, @tt{* code = $loinc#85354-9 "Blood pressure panel"}),
bindings (@tt{from VS (required)}), types (@tt{only Quantity}), slicing (@tt{contains a 1..1 and b
0..*} with @tt{^slicing} carets), @tt{obeys}, @tt{insert}, and @tt{^} caret paths for anything
else. The language reference is the @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FSH spec}.

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

@section{Using it}

@bold{A project.} The SUSHI layout: @tt{sushi-config.yaml} (canonical, fhirVersion, dependencies,
id, version) and @tt{input/fsh/*.fsh}. Dependencies are found in @tt{fhir-spec} as
@tt{<id>-<version>.tgz} and put ahead of core when names resolve.

@verbatim{
raco fsh ig/
    compile every .fsh under ig/ to ig/fsh-generated/<Type>-<id>.json
raco fsh check ig/
    validate every example against the project's profiles; problems on their FSH lines
raco fsh ig/ --pack              also write the FHIR NPM package, <id>-<version>.tgz
raco fhir-server serve-ig example.notes-0.1.0.tgz --capability <id>    serve it (fhir-server)
raco fsh --decompile StructureDefinition-x.json                          FHIR to FSH
}

@tt{check} prints each problem with its source line and a caret, naming the profile rule an example
breaks, then a summary, and exits 1 on an error:

@verbatim{
input/fsh/examples.fsh:5:3: fsh validate error: instance tumor-1: Observation.component.value[x]:
    valueString is not allowed in slice 'longest' (value[x]: Quantity)
    (constrained by profile TumorSize at input/fsh/profiles.fsh:10:3)
 5 | * component[longest].valueString = "big"
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fsh check: 3 examples, 1 with errors; 1 error(s), 0 warning(s)
}

@litchar{--format json} or @tt{outcome} gives the same to a program. Flags: @litchar{-o <dir>},
@litchar{--fhir-version 4.0.1|5.0.0}, @litchar{--canonical <url>}, @litchar{--snapshot}, @litchar{--pack [out.tgz]},
@tt{check --capability <CapabilityStatement id>} (also reports what the generated server would not
do). Exit codes: 0, 1 on an FSH error, 2 on usage.

@bold{As a module.} @racket[(require "profile.fsh")] compiles the file when the program compiles
(a parse or compile error is positioned in it) and gives @racket[document] (the entity ASTs),
@racket[compiled] (the FHIR resources as jsexprs) and @racket[resources] (typed nodes, read
leniently). The target is R5 with the default canonical; a project with a config is compiled with
@racket[compile-project]. @tt{racket profile.fsh} prints the JSON.

@racketblock[
(require (only-in "profile.fsh" [compiled clinic]))
(code:comment "the profile is now resolvable")
(parameterize ([current-data-source (resources->data-source clinic)])
  (ensure-valid reading #:profile "http://example.org/StructureDefinition/screening-blood-pressure"))
]

@bold{From Racket.}

@racketblock[
(require fsh/compile fsh/project fsh/check fsh/decompile)
(fsh->fhir text)                                   (code:comment "source -> (listof jsexpr)")
(define p (compile-project "ig/"))                 (code:comment "with its sushi-config.yaml")
(project-artifacts p) (project-diagnostics p)
(code:comment "validate in the same context")
(parameterize ([current-sd-packages (project-sd-packages p)])
  (apply-operation "$validate" resource))
(check-report-diagnostics (check-fsh-project "ig/"))
(fhir->fsh sd)                                     (code:comment "jsexpr -> FSH text")
]

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
@defproc[(parse-fsh-entity [src string?]) any/c]{One entity.}

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
                     [sd-packages (listof path?)])]{
A compiled project: its sushi-config, the source texts by file, the entities with their locations,
the resources, every diagnostic (parse errors per file; a failing entity positioned at its
declaration), the dependencies not found, and the packages the compile resolved against
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
instance rule that wrote the element and names the profile rule it breaks.}
@defstruct*[check-report ([diagnostics list?] [texts hash?] [examples exact-nonnegative-integer?]
                          [invalid exact-nonnegative-integer?] [artifacts list?])]
@defproc[(trace-rule [rules list?] [issue-path string?]) (or/c any/c #f)]{
The rule whose path best matches a validator path: the type root dropped, brackets and choice suffixes
normalised, the longest leading run of steps wins.}
