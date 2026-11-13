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

## Storing the guide

Staging the guide stages the collection: the ImplementationGuide, every definition the workspace's
files make (profiles, value sets, code systems, CQL libraries, maps) and their examples, as one
change that commits or is refused whole.

```text
stage path=sushi-config.yaml keep:=false     (check only)
stage path=sushi-config.yaml
commit stage=latest reason="first draft of the clinic guide"
```

What happens to a definition on the way in follows HL7's CRMI guide:

- **It takes the guide's facts.** Its `version`, `publisher` and `experimental` flag are the guide's
  where it states none, and its `status` is the guide's (`draft` when the guide states none). A CQL
  library that states a version of its own keeps it, and its released state with it.
- **It is held to CRMI's Shareable profile**: a url, a version, a title, a status, the experimental
  flag and a description. FSH has `Title:` and `Description:`; the guide file has `title:` and
  `description:`; a CQL library or a VCL value set gets its title from its name and its description
  from its leading comment.
- **Released, it does not change.** `status: active` in the guide releases the collection. After
  that a definition's content does not change and it does not go back to draft: raise `version`.
- **A new version keeps the released one.** With `version` raised, the stored active definition
  stays as an instance of its own (`StructureDefinition/lab-0.1.0`) beside the new draft
  (`StructureDefinition/lab`). A canonical means the active version; `<url>|0.2.0` names another
  (`describe ref=<url>|0.2.0`). Data is held to the active one once the working files are gone;
  while they are there, the working copy is what every check uses.
- **What a changed rule breaks is said.** A stored example the new rule no longer accepts refuses
  the stage, with the rule's message.

`stage path=lab.fsh definitions:=true` stages one file's own definitions (without the flag, its
instances alone).

### The store is the guide; the files are working copies

What is committed is in the store, so a working file may be deleted — or never have been here.

- **With no `sushi-config.yaml`, the stored ImplementationGuide is the workspace's guide**: its
  canonical, FHIR version, dependencies and `global` profiles still hold, and `check` says where
  they are read from (`"guide": "ImplementationGuide/example.clinic-0.1.0 (stored)"`). Of several
  stored versions it is the active one, as for any canonical.
- **Any stored definition comes back as the text that states it**, to read or to edit:

```text
describe ref=http://example.org/clinic/StructureDefinition/lab as=source      (read it)
write path=lab.fsh ref=http://example.org/clinic/StructureDefinition/lab|0.2.0 (edit it: the file is written)
write path=sushi-config.yaml ref=ImplementationGuide/example.clinic
write path=A1c.cql ref=Library/A1c
```

  What comes back is what its commit kept: the lines of the file that defined it — your comments,
  the RuleSets, Invariants and Aliases it uses, and none of the file's other entities — whether
  that file was FSH, CQL, VCL or a map; the guide is its `sushi-config.yaml`. (A guide's commit
  records each resource's own file: `SELECT type, id, source, at FROM trail` says `lab.fsh:1`.) A
  version no commit here wrote that way — a released version kept beside the next, a definition
  loaded from elsewhere — is written from the resource: FSH without the version, status and
  publisher its guide gives it, a library's CQL, a StructureMap as the mapping language (the
  specified language: `learn://fml`), the guide's `sushi-config.yaml`. `ref` is a
  canonical (`url`, or `url|version` for one that is not the active one) or a `Type/id`. The text
  compiles to what is stored: staged unchanged, it is "nothing to commit".
- **Definitions are staged under a guide being drafted.** With only a released guide in the store,
  staging a definition is refused with the call that hands the guide back; raise its `version` and
  go on.

### The store in git: the text export

A SQLite file is not something git can diff or merge, so git holds the store's definitions as text.
`workspace.yaml`'s `export: export/` (what `init` writes) names a directory that every commit of a
definition rewrites:

- the files the commits kept — `lab.fsh`, `A1c.cql`, `labs.vcl`, `sushi-config.yaml`, a JSON
  resource: the author's own text, comments included, the newest version of each file that still
  states something stored;
- one file for each stored definition no file of the workspace states (staged from text given in a
  call, or changed since by another source), written from the resource: `StructureDefinition-vital.fsh`,
  a library as `.cql`, a map as `.map`; and likewise each example the guide lists.

The directory is a mirror: the workspace never reads it back as its sources (it is the store, not
the working copy), and only files an export wrote are ever removed from it. Put back as a
workspace's files, the export compiles to what is stored (`stage path=sushi-config.yaml` answers
"nothing to commit"), which is how a store is rebuilt from git. From the shell:

```text
raco fhir-mcp export            (write it now; a commit of definitions does this by itself)
raco fhir-mcp export --check    (CI: exit 1 and `differs lab.fsh`, `missing …`, `stale …` when the directory is not the store's)
```

### What depends on what

Dependencies are traced as HL7 CRMI lists them, per type: a profile's `baseDefinition`, the types,
profiles and value sets of its differential; a value set's includes and code systems; a map's
structures and imports; a library's `relatedArtifact` and `dataRequirement`s; any resource's
`meta.profile`. FHIR's own definitions are implied and not listed. A CQL library's Library resource
states its own — each `include`, `codesystem` and `valueset` as a `depends-on` related artifact, each
retrieve as a `dataRequirement` (the type, and the code or value set it filters by).

```text
query language=sql text="SELECT type, id, path FROM dependencies WHERE target = 'http://example.org/clinic/ValueSet/labs'"
apply operation=$data-requirements inputs:='{"url": "http://example.org/clinic/Library/A1c"}'
```

`dependencies` is a table of the store: one row per stored resource and target (`type, id, url,
version, path, target, target_version`), so "what depends on this value set" — or "which stored
data claims this profile" — is a query. `$data-requirements` (CRMI's operation) answers one
artifact's needs traced to the end, as a module-definition Library: a `depends-on` entry per
dependency, at the version it resolves to (a working copy has none yet; `artifactVersion:
["<url>|<version>"]` pins one for the call), and the data its libraries retrieve.

### Pinning versions

A canonical with no version (`…/StructureDefinition/lab` in a `global` rule, a `meta.profile`, a CQL
`valueset`) means, among the stored versions, the active one. To mean another — the draft, to try
it against the data before releasing it; a fixed version of a dependency — pin it. The pins are a
Parameters resource (HL7 CRMI's *manifest parameters*), written like any resource:

```fsh
Instance: pins
InstanceOf: Parameters
* parameter[0].name = "default-canonical-version"
* parameter[0].valueCanonical = "http://example.org/clinic/StructureDefinition/lab|0.2.0"
```

`default-canonical-version` pins any definition; `default-valueset-version` a value set and
`default-system-version` a code system (`http://loinc.org|2.74`). Two ways to put them in force:

```text
stage fsh=@reading.fsh keep:=false manifest=@pins.json      (one call: any tool but write and commit takes `manifest`)
check data:=true manifest=Parameters/pins                    (a Parameters the workspace defines or stores)

parameters:                                                  (in sushi-config.yaml: every call, for everyone)
  path-expansion-params: input/resources/Parameters-pins.json
```

The guide names its pins as the IG Publisher is told where they are: the file name's
`Parameters-<id>.json` is the Parameters with that id — the FSH instance above, or a JSON file. The
compiled ImplementationGuide carries them (contained, under CRMI's `cqf-expansionParameters`), so
they hold when the guide is read from the store too. A working copy still comes first: a profile
a workspace file defines is that file, whatever is pinned. `apply operation=$resolve
inputs:='{"canonical": "<url>"}'` (CRMI's operation) answers the resource a canonical means: the
version it names, else the pinned one, else the one in force.

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
| `StructureDefinition.description: minimum required = 1, but found 0 (add `Description: "…"` to its FSH)` (staging) | add the keyword; `title:` and `description:` in sushi-config.yaml for the guide itself |
| `StructureDefinition/lab (0.1.0) is active: its content does not change — a new version is required (CRMI 3.1): raise `version` in sushi-config.yaml` | raise `version` (and set `status: draft`) to begin the next version |
| `… is active: an active artifact does not go back to draft — a new version is required …` | the same: a released version stays released |
| `definitions are staged as part of the workspace's implementation guide, whose version is theirs: write sushi-config.yaml …` | write the guide file first |
| `… the guide here is ImplementationGuide/<id> (stored), version 0.1.0, which is active — hand it back as its file and raise `version` to draft the next (write path=sushi-config.yaml ref=…)` | make the call it gives, then raise `version` |
| `path-expansion-params names Parameters/pins, which nothing in the workspace defines or the data holds …` (a warning) | write the Parameters (an FSH `Instance: pins`), or correct the name |

Not written here: pages, menus and templates (the IG Publisher's website). Not staged with the
guide yet: a capability app's statement and ViewDefinitions.
