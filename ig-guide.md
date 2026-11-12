# The implementation guide (`sushi-config.yaml`)

A workspace's definitions — its profiles, value sets, libraries, maps, views — are an
**implementation guide**: one package, one base url, one version, the packages it builds on. The
guide is a FHIR resource, ImplementationGuide, and it has a text form that already exists:
`sushi-config.yaml`, the file SUSHI reads for an FHIR Shorthand project. Here that file is a
language like the others: it compiles to the ImplementationGuide, it is checked, and the rest of
the workspace reads what it says.

References: [SUSHI configuration](https://fshschool.org/docs/sushi/configuration/) (the syntax),
[ImplementationGuide](https://hl7.org/fhir/R4/implementationguide.html) (the resource),
[CRMI](https://hl7.org/fhir/uv/crmi/) (how a guide's artifacts are versioned and released).

## Cheat sheet

```yaml
# clinic-guide
id: example.clinic                       # the package id; the resource's id
canonical: http://example.org/clinic     # the base of every url the workspace defines
name: ClinicGuide                        # letters and digits; made from the id when left out
title: Clinic Guide
status: draft                            # draft | active | retired; draft when left out
version: 0.1.0                           # every artifact has a version
fhirVersion: 4.0.1                       # 4.0.1 or 5.0.0
license: CC0-1.0
publisher:
  name: Example Clinic
  url: http://example.org
dependencies:                            # the packages the workspace builds on
  hl7.fhir.us.core: 6.1.0
global:                                  # a profile every resource of a type is held to
  Observation: http://example.org/clinic/StructureDefinition/lab
```

What each part becomes:

| In the file | In the ImplementationGuide | In the workspace |
|---|---|---|
| `id`, `canonical` | `id`, `packageId`, `url` = `<canonical>/ImplementationGuide/<id>` | the base of the urls files are given |
| `fhirVersion` | `fhirVersion` | the FHIR version every language runs in |
| `dependencies` | `dependsOn` (package id, version, the dependency's own guide url) | packages in context: their profiles, value sets and search parameters resolve |
| `global` | `global` (type, profile) | constraints: every staged resource of the type is validated against the profile |
| what the other files define | `definition.resource`: a reference, a name, whether it is an example and of which profile | — |
| `parameters` | `definition.parameter` | — |

`workspace.yaml` keeps what is not the guide's: where the data is (`data`), `audit`, CQL
`parameters`, `allowRemoteWrites`, `models`. When both files state the canonical or the FHIR
version, the guide's is used and the difference is a warning. Without a `sushi-config.yaml`, a
workspace works as before, from `workspace.yaml` alone.

## A worked example

A workspace with the guide above and one FSH file:

```fsh
Profile: Lab
Parent: Observation
Id: lab
Title: "A lab result"
* subject 1..1

Instance: a1c-1
InstanceOf: lab
* status = #final
* code = http://loinc.org#4548-4
* subject = Reference(Patient/ann)
* valueQuantity = 7.2 '%'
```

Running the guide (`run path=sushi-config.yaml`) answers what it is and what it lists:

```answer
{"summary": {"guide": "http://example.org/clinic/ImplementationGuide/example.clinic",
             "version": "0.1.0", "status": "draft", "fhirVersion": ["4.0.1"],
             "dependsOn": ["hl7.fhir.us.core#6.1.0"],
             "global": ["Observation: http://example.org/clinic/StructureDefinition/lab"],
             "resources": 2},
 "rows": [{"reference": "Observation/a1c-1", "name": "a1c-1", "example": true,
           "profile": "http://example.org/clinic/StructureDefinition/lab"},
          {"reference": "StructureDefinition/lab", "name": "A lab result", "example": false}]}
```

The profile's url is `http://example.org/clinic/StructureDefinition/lab` because the guide's
canonical is the workspace's; `Observation/a1c-1` is an example of it because the instance claims
it. `describe ref=http://example.org/clinic/ImplementationGuide/example.clinic` gives the resource.

## Running it

- **Workspace** (fhir-mcp): `write path=sushi-config.yaml text=@sushi-config.yaml` (problems in the
  answer), `check`, `run path=sushi-config.yaml`, `describe ref=<the guide's url>`.
- **Command line**: `raco fsh check <dir>` compiles the FSH of a project with its
  `sushi-config.yaml` (canonical, version, dependencies); `raco fsh <dir> --pack out.tgz` writes the
  package the guide describes.
- **Racket**: `(require fsh/ig fsh/ig-config)`, then
  `(implementation-guide (parse-ig-config text) resources)` and `(ig-diagnostics text file)`.

## Common problems

Each is reported at its line in the file.

| Message | What to do |
|---|---|
| `an implementation guide has an `id`: its package id, as in `id: example.clinic`` | add `id:` |
| `an implementation guide has a `canonical`: the base of the urls it defines, …` | add `canonical:` |
| `no `version`: every artifact has one (`version: 0.1.0`)` (a warning) | add `version:` |
| `status `actve` is not draft, active, retired or unknown (did you mean `active`?)` | fix the status |
| `package no.such.package 1.0.0 is not at hand: what it defines will not resolve …` (a warning) | put the package's `.tgz` where packages are found, or correct the id and version |
| `global: nothing defines the profile <url> that every Observation is held to` | define the profile (an FSH `Profile:` with that url), or correct the url |
| `workspace.yaml: canonical <a> differs from sushi-config.yaml's <b>, which is used` | remove `canonical` from workspace.yaml |

Not written here: pages, menus and templates (the IG Publisher's website), and version pins for
dependencies' artifacts.
