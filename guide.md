# FHIR Shorthand (FSH)

FSH is how you say what valid FHIR data looks like: profiles, extensions, value sets, code systems,
logical models, and example instances. A directory of `.fsh` files is one project. It compiles to
FHIR JSON (StructureDefinitions, ValueSets, CodeSystems, the instances), and every example instance
is validated against the profiles it claims. Each problem is reported at the rule that caused it,
with the profile rule that constrains it.

This guide is for writing FSH here: the language in brief, the ways to run it, and what to do when
something goes wrong. The specification is the reference for the language itself.

## Official references
- FHIR Shorthand specification: https://hl7.org/fhir/uv/shorthand/
- Language reference: https://hl7.org/fhir/uv/shorthand/reference.html
- FSH School (tutorials, SUSHI): https://fshschool.org/docs/

## Cheat sheet
```fsh
Alias: $loinc = http://loinc.org             // a name for a url

Profile: BloodPressurePanel                  // constrain a resource
Parent: Observation
Id: bp-panel
Title: "Blood pressure panel"
* status = #final                            // fixed value (a pattern)
* code = $loinc#85354-9                      // system#code
* subject 1..1 MS                            // cardinality, must-support
* subject only Reference(Patient)            // narrow a type
* component 2..*
* category from http://hl7.org/fhir/ValueSet/observation-category (required)   // binding
* obeys bp-1                                 // an invariant

Invariant: bp-1
Description: "A panel has a value or its components"
Expression: "value.exists() or component.exists()"
Severity: #error

Instance: bp-example                         // an example, validated against its profile
InstanceOf: BloodPressurePanel
* status = #final
* code = $loinc#85354-9
* subject = Reference(Patient/example)
* component[0].code = $loinc#8480-6          // [0], [1]: repeating elements by index
* component[0].valueQuantity = 120 'mm[Hg]'  // a UCUM quantity
* component[1].code = $loinc#8462-4
* component[1].valueQuantity = 80 'mm[Hg]'

ValueSet: HbA1cTests
Id: hba1c-tests
* $loinc#4548-4 "Hemoglobin A1c/Hemoglobin.total in Blood"
* include codes from system http://snomed.info/sct where concept is-a #44054006
```
Comments are `//` and `/* … */`. Paths are element names from the parent (`component.code`,
`value[x]`, `valueQuantity`). In instances, `[0]` indexes a repeating element, `[+]` takes the
next one and `[=]` the current one; a slice is `component[systolic]`. `Id:` gives the resource
id (and the canonical `<canonical>/StructureDefinition/<id>`); without it the name is used.

## Worked example
A profile for HbA1c results, and an example that conforms to it:
```fsh
Alias: $loinc = http://loinc.org

Profile: HbA1cObservation
Parent: Observation
Id: hba1c-observation
* status = #final
* code = $loinc#4548-4
* subject 1..1
* value[x] only Quantity

Instance: hba1c-example
InstanceOf: HbA1cObservation
* status = #final
* code = $loinc#4548-4
* subject = Reference(Patient/example)
* valueQuantity = 7.2 '%'
```
`raco fsh check` on it: one example, no errors. Change the example's status to `#preliminary` and
the check reports, at that rule, that `Observation.status` does not match the pattern `final`, and
names the profile rule that set it.

## Running it
**Command line.** `fsh` (or `raco fsh`):
```
fsh check dir/                      # compile + validate every example; problems at their rules; exit 1 on an error
fsh dir/ -o out/                    # the resources as FHIR JSON files (default: dir/fsh-generated)
fsh dir/ --pack out.tgz             # a FHIR NPM package (needs sushi-config.yaml)
fsh --decompile resource.json       # FHIR -> FSH (GoFSH)
```
A project's `sushi-config.yaml` (as SUSHI reads it) gives its `canonical`, `fhirVersion` and
`dependencies`; the dependency packages come from fhir-spec or the package directories, so a
`Parent` can be a dependency's profile (`USCorePatientProfile`). Without one, `--canonical` and
`--fhir-version` set them. `--format json` (or `outcome`) puts the diagnostics on stdout.

**As a module.** A file that starts with `#lang fsh` is a Racket module providing `compiled` (the
resources as JSON), `resources` (typed nodes) and `document` (the FSH AST); its profiles, value sets
and code systems are registered when it is required, so other modules find them by canonical.

**In a workspace** (fhir-mcp). The workspace's `.fsh` files are one project, compiled with
`workspace.yaml`'s canonical, FHIR version and packages. `write` answers a file's problems at their
lines; `check` checks the whole project. `run` of a file gives the resources its entities compile
to, each with its problems; `explain` of an entity gives its resource and its problems, an
example's validation issues traced to the rules that wrote the elements. `describe("Patient")`
lists a type's elements. What the FSH defines, the other languages see by canonical: a CQL library
uses its value sets, a VCL file is a value set beside it.

## Common problems
- `` `nmae` is not an element of Patient ``: a misspelled path, at its rule. The first step of a
  path must be an element of the Parent.
- `Observation.status: value does not match required pattern "final" (constrained by profile …)`:
  the example contradicts a fixed value in its profile; the message names the profile's rule.
- `` Parent `X` not found ``: nothing defines the Parent — not a FHIR type, not a profile,
  extension or logical model in the project, not in the packages. Check the name, or add the
  package that defines it to `dependencies`. The profile is not compiled.
- `the StructureDefinition id `x` is already used (file:line)`: two definitions share an id (the
  name, when there is no `Id:`), so one would replace the other. Give one of them its own `Id:`.
  Instances too: `the Patient instance id `ann` is already used` — two instances of one type (or
  profile) with one id, often a file of data written beside a project's examples.
- `` unexpected sequence `%` — a quantity's UCUM unit is quoted: `7.1 '%'` ``: a unit written bare
  after a number (`* valueQuantity = 7.1 %`); FSH quotes it, `7.1 '%'`, or `7.1 'mm[Hg]' "mmHg"`.
- `` RuleSet `X` not found ``: an `insert` of a RuleSet no file of the project defines (a typo in
  its name, or a file missing from the directory).
- An example with `minimum required = 2, but found 0`: the profile's cardinality (`component 2..*`)
  — the example must have the elements its profile requires.
- `valueString is not allowed in slice 'diastolic' (value[x]: Quantity)`: an entry of a slice
  uses a type the slice's `only` rule excludes. The error sits on the instance rule that wrote the
  entry and names the slice's rule in the profile.
- `fsh parse error: unexpected sequence `#``: a `#` comment; FSH comments are `//`.
