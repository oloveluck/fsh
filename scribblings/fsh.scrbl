#lang scribble/manual
@; Text in @tt is shown exactly as written: no curly quotes, and "--" stays two hyphens.
@(require (only-in scribble/core [element literal-element]))
@(define (tt . text) (literal-element (quote tt) text))
@(require (for-label racket/base racket/contract
                     fsh/parser/parse fsh/lint fsh/compile fsh/decompile fsh/print fsh/project fsh/pack fsh/check fsh/ig-config
                     fsh/ig fsh/workspace fsh/support
                     (only-in fhir/diagnostic diagnostic?) (only-in fhir/lang/workspace ws-support?)
                     (only-in fhir/lang/support lang-support?)))
@; c : a piece of FSH or JSON inside a sentence, typeset as written (tt would curl its quotes)
@(define (c . strs) (elem #:style 'tt (apply literal strs)))

@title{fsh: FHIR Shorthand}

@hyperlink["https://hl7.org/fhir/"]{FHIR}, the standard health systems use to exchange data,
describes a lab result or a patient in general terms. Almost every field is optional, so that one
kind of record, the Observation, can hold a blood pressure in one clinic and a genetic finding in
another. People who actually exchange data have to say more: "here, a lab result always names its
patient, is final, and carries its value as a number with a unit". A set of rules like that is
called a @italic{profile}. A profile tells a sender what to send and a receiver what to expect, and
it lets a program decide whether a record is acceptable.

A profile is itself a FHIR resource, and written out as JSON it is long and hard to read. So people
write profiles in @hyperlink["https://hl7.org/fhir/uv/shorthand/"]{FHIR Shorthand} (FSH), an HL7
standard: a small text language with one line per rule. FSH also covers what a profile needs around
it: the sets of codes a field may hold, fields FHIR does not have, and @italic{instances}, which are
examples of the data itself. Profiles, code sets and examples published together for one purpose
are an @italic{Implementation Guide}; US Core and the International Patient Summary are two.

This package is a compiler for FSH. It turns @tt{.fsh} files into the FHIR resources they stand for.
It checks every example against the profiles and reports each broken rule at the FSH line that
caused it. It also goes the other way, from FHIR JSON back to FSH. It reads the same files as
@hyperlink["https://fshschool.org/docs/sushi/"]{SUSHI}, the compiler HL7 maintains and most guides
are built with, so a project written for SUSHI can be checked here as it is.

Part of the @hyperlink["https://oloveluck.github.io/fhir/"]{fhir} family of FHIR languages and tools.

@table-of-contents[]

@section{What you can do with it}

@itemlist[
@item{@bold{Write a rule for your data and see whether an example passes}: @secref["first"].}
@item{@bold{Find a mistake where it is}: a misspelt field, an example that breaks its profile, a
unit written wrongly are each reported at their line, usually with the fix (@secref["check"]).}
@item{@bold{Build on a published guide}: start from US Core's patient instead of FHIR's, and be held
to its rules too (@secref["project"]).}
@item{@bold{Get the FHIR JSON, or a package of it}, to load into a server or hand to another tool
(@secref["output"]).}
@item{@bold{Turn FHIR JSON you already have into FSH}, to read it or to keep editing it as text
(@secref["decompile"]).}
@item{@bold{Use a profile from a program}: a @tt{.fsh} file is a module a Racket program can require
(@secref["module"]).}]

@section[#:tag "first"]{A first example}

@bold{1. Install} the tools as the @hyperlink["https://oloveluck.github.io/fhir/"]{family's front
page} describes. The command is then @tt{raco fsh} (or @tt{fsh}, if you built the standalone tools).

@bold{2. Write a profile and an example of it.} Make a folder, @tt{clinic/}, and in it a file
@tt{lab.fsh}. The profile says what an HbA1c result (a blood test used to follow diabetes) must look
like in this clinic; the instance is one such result.

@verbatim|{Alias: $loinc = http://loinc.org

Profile: HbA1cResult
Parent: Observation
Id: hba1c-result
Title: "HbA1c result"
Description: "A final HbA1c result that names its patient and has a measured value."
* status = #final
* code = $loinc#4548-4
* subject 1..1
* value[x] 1..1
* value[x] only Quantity

Instance: ann-hba1c
InstanceOf: HbA1cResult
Description: "Ann's HbA1c of 7.2 %"
* status = #final
* code = $loinc#4548-4
* subject = Reference(Patient/ann)
* valueQuantity = 7.2 '%'}|

Reading it from the top:

@itemlist[
@item{@tt{Alias} gives a short name to a long address. LOINC is the catalogue of codes for lab
tests; @c{$loinc#4548-4} is "code 4548-4 from LOINC", the code for HbA1c.}
@item{@tt{Profile} starts a set of rules and names it. @tt{Parent: Observation} says the rules are
about FHIR's Observation, the kind of record a measurement is kept in. @tt{Id} is the short name it
is published under.}
@item{Each line that starts with @tt{*} is one rule. @c{* status = #final} fixes a value: only
final results. @c{* subject 1..1} is a count, "at least one, at most one": the result must name its
patient. The last two rules say there must be a value and that it must be a Quantity, a number with
a unit. (@tt{value[x]} is FHIR's way of writing a field that can hold several kinds of value.)}
@item{@tt{Instance} starts an example: a real record, written field by field.
@tt{InstanceOf: HbA1cResult} claims that it follows the profile. Because the value is a Quantity,
the field is written @tt{valueQuantity}; the unit goes in single quotes.}]

@bold{3. Check it.}

@verbatim|{
$ raco fsh check clinic/
fsh check: 1 example, 0 with errors; 0 errors, 0 warnings
  canonical: http://example.org (default; a sushi-config.yaml beside the files sets it: `canonical: http://…`)
}|

@tt{check} compiled the file and then held the example to the profile: one example, nothing wrong.
The second line is about names. Every definition is published under a web address, its
@italic{canonical url}; with nothing to say otherwise, this profile is
@tt{http://example.org/StructureDefinition/hba1c-result}. @secref["project"] shows how to set your
own.

@bold{4. Make a mistake.} Change the example's status to @c{#preliminary} and check again:

@verbatim|{
$ raco fsh check clinic/
clinic/lab.fsh:17:3: fsh validate error: instance ann-hba1c: Observation.status: value does not match required pattern "final" (constrained by profile HbA1cResult at line 8)
 17 | * status = #preliminary
    |   ^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
  canonical: http://example.org (default; a sushi-config.yaml beside the files sets it: `canonical: http://…`)
}|

The message has three parts: where the bad value was written (line 17, with a marker under it), what
is wrong with it (the status is not @tt{final}), and which rule of the profile says so (line 8).
The command ends with exit status 1, so a script can tell. Put @c{#final} back.

@bold{5. Get the FHIR.} Without @tt{check}, the command writes the resources as JSON files:

@verbatim|{
$ raco fsh clinic/
  StructureDefinition-hba1c-result.json
  Observation-ann-hba1c.json
fsh: wrote 2 artifact(s) to clinic/fsh-generated
}|

The first file is the profile as FHIR knows it, a @italic{StructureDefinition}. The second is the
example as an ordinary FHIR record:

@verbatim|{{"code":{"coding":[{"code":"4548-4","system":"http://loinc.org"}]},"id":"ann-hba1c",
 "meta":{"profile":["http://example.org/StructureDefinition/hba1c-result"]},
 "resourceType":"Observation","status":"final","subject":{"reference":"Patient/ann"},
 "valueQuantity":{"code":"%","system":"http://unitsofmeasure.org","unit":"%","value":7.2}}}|

(The file is one line; it is broken here to fit.) Any FHIR server or validator can read both.

@section[#:tag "writing"]{Writing FSH}

This section goes through the parts of the language you will use first, each with an example. The
full language is described in the @hyperlink["https://hl7.org/fhir/uv/shorthand/reference.html"]{FSH
reference}, and @hyperlink["https://fshschool.org/docs/"]{FSH School} has tutorials.

@subsection{The shape of a file}

A file is a list of @italic{entities}. Each starts with a line saying what it is and what it is
called, then a few keyword lines (@tt{Parent:}, @tt{Id:}, @tt{Title:}, @tt{Description:}), then its
rules, one per @tt{*}. Comments are @tt{//} to the end of the line, or @tt{/* … */}. A blank line
between entities is customary.

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{entity} @bold{what it defines})
      (list @tt{Profile} "rules for one kind of record")
      (list @tt{Instance} "one record: an example, or a definition written out field by field")
      (list @tt{ValueSet} "a set of codes a field may hold")
      (list @tt{CodeSystem} "codes of your own")
      (list @tt{Extension} "a field FHIR does not have")
      (list @tt{Invariant} "a rule written as a test, used by a profile")
      (list @tt{RuleSet} "rules to reuse in several places")
      (list @tt{Alias} "a short name for an address")
      (list @tt{Logical} "a data structure that is not a FHIR resource, such as a form")
      (list @tt{Resource} "a new kind of resource")
      (list @tt{Mapping} "how a profile's fields correspond to another standard's"))]

All the files in one folder (and its subfolders) are one @italic{project}: an entity in one file can
use an entity in another.

@subsection[#:tag "profiles"]{Profiles: rules for a kind of record}

A profile starts from a parent and narrows it. The parent is a FHIR type (@tt{Patient},
@tt{Observation}) or another profile, yours or a published one. Every rule begins with a
@italic{path}, the name of the field it is about; a field inside a field is written with a dot
(@tt{name.family}).

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{rule} @bold{example} @bold{meaning})
      (list "count" @c{* subject 1..1} "how many: at least 1, at most 1")
      (list "flag" @c{* subject 1..1 MS} "MS: a system must be able to handle this field")
      (list "fixed value" @c{* code = $loinc#85354-9 "Blood pressure panel"} "the field must contain this")
      (list "exact value" @c{* status = #final (exactly)} "the field must be this and nothing more")
      (list "type" @c{* value[x] only Quantity or CodeableConcept} "which kinds of value are allowed")
      (list "reference" @c{* subject only Reference(Patient)} "what a link may point to")
      (list "code set" @c{* category from ObservationCategoryCodes (required)} "the codes allowed (a binding)")
      (list "slices" @c{* component contains systolic 1..1 and diastolic 1..1} "named entries of a list")
      (list "invariant" @c{* obeys bp-1} "a test the record must pass")
      (list "rule set" @c{* insert CommonMetadata} "the rules of a RuleSet, here")
      (list "anything else" @c{* code ^short = "What was measured"} "set a property of the field's definition"))]

A code is written @c{system#code}, with the system's address or an alias for it; a code whose
system is already known from the field is just @c{#final}.

A path the parent does not have is caught when the profile is compiled, with the name you probably
meant:

@verbatim|{
$ raco fsh clinic/
clinic/lab.fsh:10:3: fsh compile error: profile: HbA1cResult: `subjct` is not an element of Observation (did you mean `subject`?)
 10 | * subjct 1..1
    |   ^^^^^^
}|

@subsection[#:tag "instances"]{Instances: examples of the data}

An instance is a record written one field per rule. It is how you show what conforming data looks
like, and it is what @tt{check} tests your profiles with: a profile nobody has tried on an example
is a guess.

@verbatim|{Instance: ann-hba1c
InstanceOf: HbA1cResult
* status = #final
* code = $loinc#4548-4
* subject = Reference(Patient/ann)
* valueQuantity = 7.2 '%'}|

@itemlist[
@item{@bold{What it is an instance of.} @tt{InstanceOf} names a profile or a plain FHIR type. With a
profile, the record says so itself (in @tt{meta.profile}) and is checked against it. Fixed values the
profile requires are filled in for you when you leave them out.}
@item{@bold{Values.} Text in double quotes, @c{"Lee"}; a date as text, @c{"1974-12-25"}; a code,
@c{#final} or @c{$loinc#4548-4}; a number with a unit, @c{7.2 '%'} or @c{150 'mm[Hg]'}; a link to
another record, @c{Reference(Patient/ann)}.}
@item{@bold{Lists.} A field that repeats is indexed: @tt{name[0].given}, @tt{name[1].given}.
@tt{[+]} means "the next entry" and @tt{[=]} "the same entry as the line above", so you need not
count. With no index you are writing the first entry.}
@item{@bold{Fields with several possible types.} Where FHIR writes @tt{value[x]}, an instance names
the type it uses: @tt{valueQuantity}, @tt{valueString}, @tt{effectiveDateTime}.}
@item{@bold{What it is for.} @c{Usage: #example} is the default. @c{Usage: #definition} marks a
record that is part of the guide itself (a search parameter, a server's capability statement), and
@c{Usage: #inline} one that only exists inside another.}]

A required field left out is reported at the instance's first line, since no rule wrote it:

@verbatim|{
clinic/lab.fsh:14:1: fsh validate error: instance ann-hba1c: Observation.subject: minimum required = 1, but found 0 (no rule of ann-hba1c writes `subject`) (constrained by profile HbA1cResult at line 10)
 14 | Instance: ann-hba1c
    | ^^^^^^^^^
}|

@subsection[#:tag "valuesets"]{Value sets and code systems: which codes are allowed}

A coded field (a visit's type, a patient's sex) holds a code from some catalogue. A
@italic{code system} is a catalogue; a @italic{value set} is a selection from one or more
catalogues; a @italic{binding} ties a field to a value set.

@verbatim|{CodeSystem: VisitKind
Id: visit-kind
Title: "Kinds of visit"
* #walk-in "Walk-in"
* #booked "Booked"
* #phone "By telephone"

ValueSet: InPersonVisits
Id: in-person-visits
Title: "Visits in person"
* VisitKind#walk-in
* VisitKind#booked

Profile: ClinicVisit
Parent: Encounter
Id: clinic-visit
* type 1..1
* type from InPersonVisits (required)}|

The code system defines three codes of the clinic's own. The value set picks two of them. The
profile's last rule binds the visit's @tt{type} to that value set. The word in parentheses is the
binding's strength: @tt{required} (only these codes), @tt{extensible} (these, unless none fits),
@tt{preferred} or @tt{example}.

A value set can also be defined by a rule over a large catalogue instead of a list:
@c{* include codes from system http://snomed.info/sct where concept is-a #44054006}.

FHIR's own required code sets are checked on every example. An Observation whose status is
misspelt, @c{* status = #finl}, gets a message that lists what is allowed:

@verbatim|{
codes/typo.fsh:3:3: fsh validate error: instance typo: Observation.status: code `finl` not in required value set http://hl7.org/fhir/ValueSet/observation-status (did you mean `final`?); it holds amended, cancelled, corrected, entered-in-error, final, preliminary, registered, unknown
 3 | * status = #finl
   |   ^^^^^^
}|

A binding your own profile adds is written into the profile, but @tt{check} does not yet test an
example's code against it (@secref["limits"]).

@subsection[#:tag "invariants"]{Invariants: a rule as a test}

Some rules are not about one field: "a heart rate is between 20 and 250". An @italic{invariant}
states such a rule twice, in words for people and as a
@hyperlink["https://oloveluck.github.io/fhirpath/"]{FHIRPath} expression for the computer. A profile
adopts it with @tt{obeys}.

@verbatim|{Profile: HeartRate
Parent: Observation
Id: heart-rate
* code = http://loinc.org#8867-4
* value[x] only Quantity
* obeys hr-range

Invariant: hr-range
Description: "A heart rate is between 20 and 250 /min"
Expression: "value.ofType(Quantity).value >= 20 and value.ofType(Quantity).value <= 250"
Severity: #error

Instance: fast
InstanceOf: HeartRate
* status = #final
* code = http://loinc.org#8867-4
* valueQuantity = 300 '/min'}|

@verbatim|{
$ raco fsh check heart/
heart/hr.fsh:17:3: fsh validate error: instance fast: Observation: hr-range: A heart rate is between 20 and 250 /min (found valueQuantity.value = 300)
 17 | * valueQuantity = 300 '/min'
    |   ^^^^^^^^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
  …
}|

The message gives the invariant's own words, then what it found, at the line that wrote it.

@subsection[#:tag "slicing"]{Slices: rules for one entry of a list}

A blood pressure is one record with two measurements in a list, systolic and diastolic. To make
rules about "the systolic entry" you first have to say how to recognise it. That is
@italic{slicing}: the list is cut into named slices, told apart by some field, here each entry's
code.

@verbatim|{Alias: $loinc = http://loinc.org

Profile: BloodPressure
Parent: Observation
Id: blood-pressure
* code = $loinc#85354-9
* component ^slicing.discriminator.type = #pattern
* component ^slicing.discriminator.path = "code"
* component ^slicing.rules = #open
* component contains systolic 1..1 and diastolic 1..1
* component[systolic].code = $loinc#8480-6
* component[systolic].value[x] only Quantity
* component[diastolic].code = $loinc#8462-4
* component[diastolic].value[x] only Quantity

Instance: reading-1
InstanceOf: BloodPressure
* status = #final
* code = $loinc#85354-9
* component[systolic].valueQuantity = 150 'mm[Hg]'
* component[diastolic].valueString = "ninety"}|

The three @tt{^slicing} lines say the entries are told apart by their @tt{code} and that other
entries are allowed (@c{#open}). @tt{contains} names the slices and says each must be there once.
After that @tt{component[systolic]} is a path like any other, in the profile and in the instance.
The instance does not repeat the slices' codes: the compiler fills them in from the profile.

@verbatim|{
$ raco fsh check bp/
bp/bp.fsh:21:3: fsh validate error: instance reading-1: Observation.component.value[x]: valueString is not allowed in slice 'diastolic' (value[x]: Quantity) (constrained by profile BloodPressure at line 14)
 21 | * component[diastolic].valueString = "ninety"
    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
  …
}|

@subsection[#:tag "extensions"]{Extensions: a field FHIR does not have}

When the data has something FHIR has no field for, you define an @italic{extension} and add it to a
profile under a name of your choosing.

@verbatim|{Extension: PreferredPharmacy
Id: preferred-pharmacy
Title: "Preferred pharmacy"
Description: "The pharmacy the patient wants prescriptions sent to."
* value[x] only Reference(Organization)

Profile: ClinicPatient
Parent: Patient
Id: clinic-patient
* extension contains PreferredPharmacy named pharmacy 0..1
* name 1..*

Instance: ann
InstanceOf: ClinicPatient
* name.family = "Lee"
* name.given = "Ann"
* extension[pharmacy].valueReference = Reference(Organization/corner-pharmacy)}|

In the compiled record the extension appears under its own address, which is how any FHIR system
recognises it:

@verbatim|{"extension": [{"url": "http://example.org/StructureDefinition/preferred-pharmacy",
               "valueReference": {"reference": "Organization/corner-pharmacy"}}]}|

@subsection[#:tag "rulesets"]{Saying it once: aliases and rule sets}

An @tt{Alias} names an address. A @tt{RuleSet} names a group of rules, which @c{* insert} puts
wherever they are wanted. A rule set can take parameters, written in braces where they are used,
which makes many similar examples short:

@verbatim|{Alias: $loinc = http://loinc.org

RuleSet: HbA1c(patient, date, value)
* status = #final
* code = $loinc#4548-4
* subject = Reference(Patient/{patient})
* effectiveDateTime = "{date}"
* valueQuantity = {value} '%'

Instance: ann-march
InstanceOf: Observation
* insert HbA1c(ann, 2025-03-01, 7.2)

Instance: ann-september
InstanceOf: Observation
* insert HbA1c(ann, 2025-09-01, 9.4)}|

Inserting a rule set the project does not define is an error at the @tt{insert}:
@c{RuleSet `Hba1c` not found: no RuleSet of that name is in the project}.

@section[#:tag "project"]{Projects and Implementation Guides}

A folder of @tt{.fsh} files is a project. It becomes an Implementation Guide when it says what it
is called, where it is published and what it builds on. That is the job of one more file.

@subsection[#:tag "ig"]{The guide file: @tt{sushi-config.yaml}}

@tt{sushi-config.yaml} sits beside the @tt{.fsh} files (or one or two folders above them). The name
comes from SUSHI, which reads the same file. A small one:

@verbatim|{id: example.clinic
canonical: http://example.org/clinic
name: ClinicGuide
title: Clinic Guide
status: draft
version: 0.1.0
fhirVersion: 4.0.1}|

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{key} @bold{what it says})
      (list @tt{id} "the guide's package name, the name it is distributed under")
      (list @tt{canonical} "the base of every address the project defines")
      (list @tt{name}  "a name a program can use: letters and digits")
      (list @tt{title} "a name for people")
      (list @tt{status} @elem{@tt{draft}, @tt{active} or @tt{retired}; your definitions carry it too})
      (list @tt{version} "the guide's version")
      (list @tt{fhirVersion} @elem{which FHIR the project is written for: @tt{4.0.1} (R4) or @tt{5.0.0} (R5)})
      (list @tt{dependencies} "the published guides the project builds on, each with its version")
      (list @elem{@tt{license}, @tt{description}, @tt{publisher}} "carried into the package"))]

With this file in @tt{clinic/}, the profile of the first example is compiled for FHIR R4 and
published as @tt{http://example.org/clinic/StructureDefinition/hba1c-result}:

@verbatim|{
$ raco fsh check clinic/
fsh check: 1 example, 0 with errors; 0 errors, 0 warnings
  canonical: http://example.org/clinic (…/clinic/sushi-config.yaml)
}|

Without the file, a project is compiled for FHIR R5 under @tt{http://example.org};
@litchar{--canonical} and @litchar{--fhir-version} on the command line set either one, and override
the file.

The guide file is a language of its own here, in a small way. FHIR has a resource that describes a
guide, the ImplementationGuide: its name and version, what it depends on, and a list of everything
it defines, marking which records are examples and of which profile. The file compiles to that
resource, and a mistake in the file is reported at its line, as in the other languages:

@verbatim|{
sushi-config.yaml:2:1: ig check error: status `actve` is not draft, active, retired or unknown (did you mean `active`?)
}|

That compilation is used where a guide is kept and versioned as a whole: in a
@hyperlink["https://oloveluck.github.io/fhir/fhir-mcp/index.html"]{fhir-mcp} workspace, where the
guide file also sets the FHIR version, the packages and the rules for every other language in the
workspace, and from Racket (@racketmodname[fsh/ig]). The command line reads the file for the facts
above and does not write the ImplementationGuide resource. The details, including two more keys
(@tt{global}, a profile every record of a type is held to, and @tt{parameters}), storing a guide,
releasing a version and pinning the versions of what it uses, are in @tt{ig-guide.md} in the
repository; @tt{raco fhir guide ig} prints it.

@subsection[#:tag "dependencies"]{Building on a published guide}

Most guides do not start from bare FHIR. In the United States a patient profile usually starts from
US Core's. Name the guide under @tt{dependencies} and its profiles, extensions and value sets can
be used by name:

@verbatim|{id: example.uscore
canonical: http://example.org/uscore
version: 0.1.0
fhirVersion: 4.0.1
dependencies:
  hl7.fhir.us.core: 6.1.0}|

@verbatim|{Profile: ClinicPatient
Parent: USCorePatientProfile
Id: clinic-patient
* birthDate 1..1

Instance: ann
InstanceOf: ClinicPatient
* name.family = "Lee"
* gender = #female
* birthDate = "1974-12-25"}|

Your profile adds one rule, and inherits all of its parent's. The example is held to both:

@verbatim|{
$ raco fsh check uscore/
uscore/patient.fsh:6:1: fsh validate error: instance ann: Patient.identifier: minimum required = 1, but found 0 (no rule of ann writes `identifier`)
 6 | Instance: ann
   | ^^^^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
  …
}|

US Core requires every patient to have an identifier, so the example needs one.

A dependency is a @italic{package}: the @tt{.tgz} file a guide is distributed as. Packages are not
downloaded for you. A dependency @tt{hl7.fhir.us.core: 6.1.0} is looked for as the file
@tt{hl7.fhir.us.core-6.1.0.tgz}, among the packages that come with the tools (US Core 6.1.0 and
8.0.0, mCODE 4.0.0, Genomics Reporting 2.0.0, CRMI 2.0.0 and CQF Measures 5.0.0) and then in the
folders named by the environment variable @tt{FHIR_PACKAGE_PATH}. Packages are published at
@hyperlink["https://packages.fhir.org"]{packages.fhir.org}. When one is missing, @tt{raco fsh} says so
and goes on, and a parent or type that needed it is then reported as not found:

@verbatim|{
fsh: dependency no.such.package 1.0.0 not found (looked for no.such.package-1.0.0.tgz in fhir-spec); its profiles will not resolve
}|

@subsection[#:tag "output"]{The FHIR JSON, and a package}

@tt{raco fsh clinic/} writes one JSON file per resource into @tt{clinic/fsh-generated/}
(@litchar{-o} names another folder). If anything fails to compile, the problems are printed and
nothing is written.

@litchar{--pack} also writes the project as a package, the form in which a guide is handed to
other tools. It needs the guide file, for the package's name and version:

@verbatim|{
$ raco fsh clinic/ --pack clinic-0.1.0.tgz
fsh: packed 2 resource(s) into …/clinic-0.1.0.tgz
  StructureDefinition-hba1c-result.json
  Observation-ann-hba1c.json
fsh: wrote 2 artifact(s) to clinic/fsh-generated
}|

Inside the package the definitions are in @tt{package/}, the examples in @tt{package/example/},
and @tt{package/package.json} carries the name, version and dependencies. Without a file name the
package is written as @tt{<id>-<version>.tgz} in the current folder.

A package that contains a description of a server (a CapabilityStatement) can be run as one:
@tt{raco fhir-server serve-ig clinic-0.1.0.tgz} starts a FHIR server that accepts what the guide
describes. See @hyperlink["https://oloveluck.github.io/fhir/fhir-server/index.html"]{fhir-server}.

@section[#:tag "running"]{Running it}

@subsection[#:tag "cli"]{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco fsh check clinic/} "compile, then test every example against the profiles; each problem at its FSH line")
      (list @tt{raco fsh check bad/ --context profiles/} @elem{check one folder against another folder's definitions; repeat @litchar{--context} for more})
      (list @tt{raco fsh check clinic/ --capability <id>} "also report what a server generated from the guide would not do")
      (list @tt{raco fsh clinic/} @elem{compile every @tt{.fsh} under the folder to @tt{clinic/fsh-generated/<Type>-<id>.json}})
      (list @tt{raco fsh clinic/ -o out/} "the same, into another folder")
      (list @tt{raco fsh clinic/ --pack [out.tgz]} "also write the package")
      (list @tt{raco fsh clinic/ --snapshot} "also write each profile's snapshot (see below)")
      (list @tt{raco fsh lab.fsh} "one file")
      (list @tt{raco fsh --decompile a.json b.json} "FHIR JSON to FSH, printed"))]

Other options: @litchar{--fhir-version 4.0.1|5.0.0} and @litchar{--canonical <url>}, for a project
without a guide file or to override it; @litchar{--format text|json|outcome}, how problems are
reported. @tt{text} is for a person and goes to the error stream; @tt{json} and @tt{outcome} (a
FHIR OperationOutcome) go to standard output for a program:

@verbatim|{{"diagnostics":[{"column":1,"lang":"fsh","line":1,"message":"instance no-value: Observation.value[x]: minimum required = 1, but found 0 (no rule of no-value writes `value`) (constrained by profile HbA1cResult at profiles/lab.fsh:11:3)","phase":"validate","position":1,"severity":"error","source":"bad/no-value.fsh","span":9}]}}|

The command exits with 0 when all is well, 1 when there is an error in the FSH and 2 when it was
called wrongly (a folder that is not there, an unknown option).

A profile is normally written as its @italic{differential}: only what it changes in its parent.
@litchar{--snapshot} adds the @italic{snapshot}, the whole resulting definition with every field,
which some tools want.

@subsection[#:tag "check"]{What @tt{check} checks}

@tt{raco fsh check} does everything this package knows how to do with a project, and reports all of
it in one run:

@itemlist[
@item{@bold{The text parses.} A parse error says what was unexpected, and where it can, what to
write instead: @c{unexpected sequence `%` — a quantity's UCUM unit is quoted: `7.2 '%'`}.}
@item{@bold{The definitions compile.} A field the parent does not have, a @tt{Parent} or
@tt{InstanceOf} nothing defines, a rule set that is not there, two definitions with the same id.
Each is reported and the rest still compiles.}
@item{@bold{Every example is valid}: against FHIR's own definition of its type, against the
profile it is an instance of, and against that profile's parents, a dependency's included. That
covers counts, fixed values, types, FHIR's required code sets, slices, invariants, the form of
dates and other simple values, and units.}
@item{@bold{Maps}, if the project has any (@secref["maps"]).}
@item{@bold{A server}, with @litchar{--capability} (below).}]

A problem in an example is placed on the rule of the instance that wrote the offending value, and
names the rule of the profile that it breaks. When the cause is something missing, there is no
such rule, so the problem is placed on the instance's first line.

@bold{Examples that are meant to fail.} It is useful to keep examples the profile should refuse.
Kept in the project they would make every check fail, so put them in a folder of their own and
check it against the project's definitions:

@verbatim|{
$ raco fsh check bad/ --context profiles/
bad/no-value.fsh:1:1: fsh validate error: instance no-value: Observation.value[x]: minimum required = 1, but found 0 (no rule of no-value writes `value`) (constrained by profile HbA1cResult at profiles/lab.fsh:11:3)
 1 | Instance: no-value
   | ^^^^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
  …
}|

The context folder's definitions are used; its own examples and problems are left out. If you
forget the option, the message tells you: @c{(`HbA1cResult` is defined in profiles/lab.fsh: check
with `--context profiles`)}.

@bold{A server's requirements.} A guide can include a CapabilityStatement: a list of what a server
that follows the guide must do (which records it serves, which searches and operations).
@litchar{--capability <id>} generates such a server from the project and reports each requirement it
would not meet. It needs the guide file and the fhir-server package.

@verbatim|{
$ raco fsh check clinic/ --capability clinic-server
clinic/server.fsh:1:1: fsh server error: clinic-server: SHALL Observation stats not met: operation not implemented
 1 | Instance: clinic-server
   | ^^^^^^^^^
fsh check: 1 example, 0 with errors; 1 error, 0 warnings
  …
}|

@subsection[#:tag "decompile"]{FHIR JSON back to FSH}

You may have FHIR JSON and no FSH: a profile somebody published, a record from a server.
@litchar{--decompile} writes it as FSH, to read or to go on editing as text. (The HL7 tool that does
this is called GoFSH.)

@verbatim|{
$ raco fsh --decompile clinic/fsh-generated/Observation-ann-hba1c.json
Instance: ann-hba1c
InstanceOf: Observation
Usage: #example
* code = http://loinc.org#4548-4
* meta.profile[0] = "http://example.org/clinic/StructureDefinition/hba1c-result"
* status = #final
* subject = Reference(Patient/ann)
* valueQuantity = 7.2 '%'
}|

Give it several files and you get one FSH document. The aim is FSH that compiles back to exactly
the JSON it came from. Where a short form would lose something, the longer form is written: the
profile of the first example comes back with its fixed values as @tt{^} rules
(@c{* status ^patternCode = "final"}), which mean the same as @c{* status = #final}.

@subsection[#:tag "module"]{As a module, from Racket}

A @tt{.fsh} file whose first line is @tt{#lang fsh} is a Racket module, so a program can require it
and use what it defines. The file is compiled when the program is built: a misspelt field in a
profile stops the build, at its line. (@tt{raco fsh} reads a file with or without that first line.)

@racketblock[
(require (only-in "lab.fsh" compiled canonical-of)
         (only-in fhir/conformance/operations ensure-valid))
(for/list ([r compiled]) (list (hash-ref r 'resourceType) (hash-ref r 'id)))
(code:comment "'((\"StructureDefinition\" \"hba1c-result\") (\"Observation\" \"ann-hba1c\"))")
(canonical-of "HbA1cResult")
(code:comment "\"http://example.org/StructureDefinition/hba1c-result\"")
(define reading (cadr compiled))
(ensure-valid reading #:profile (canonical-of "HbA1cResult"))
(code:comment "returns the record; raises, listing the problems, when it is not valid")
]

The module provides @racket[compiled] (the resources as JSON values), @racket[resources] (the same
as typed nodes), @racket[document] (the parsed entities) and @racket[canonical-of] (a definition's
address, by its name or id). Requiring it also registers its profiles, value sets and code systems
under their addresses, which is why @racket[ensure-valid] above can find the profile. Like the
command line, the module is compiled with the nearest @tt{sushi-config.yaml}. Running the file,
@tt{racket lab.fsh}, prints its resources as JSON.

Building a module does not validate its examples; that needs the whole project. To have them
checked on every test run, call the checker from a test:

@racketblock[
(require fsh/check (only-in fhir/diagnostic diagnostic->string))
(map diagnostic->string (check-report-diagnostics (check-fsh-project "clinic/")))
(code:comment "'() when every example is valid; else one line per problem")
]

@subsection[#:tag "racket"]{The pieces, from Racket}

The steps the command line does, one at a time. Load a FHIR version's types first
(@racketmodname[fhir/r5/schemas] or @racketmodname[fhir/r4/schemas]) when you compile text
yourself; @racket[compile-project] and @racket[check-fsh-project] do that for you.

@racketblock[
(require fhir/r5/schemas fsh/compile fsh/project fsh/check fsh/decompile)
(fsh->fhir text)                                   (code:comment "FSH text -> the resources, as JSON values")
(define p (compile-project "clinic/"))             (code:comment "a project, with its sushi-config.yaml")
(project-artifacts p)                              (code:comment "the resources")
(project-diagnostics p)                            (code:comment "every problem, positioned")
(check-report-diagnostics (check-fsh-project "clinic/"))  (code:comment "what raco fsh check reports")
(fhir->fsh resource)                               (code:comment "a JSON value -> FSH text")
]

@subsection[#:tag "maps"]{Maps in a project}

A project may also hold @tt{.map} files in the
@hyperlink["https://oloveluck.github.io/fml/"]{FHIR Mapping Language}, which converts data from
one structure to another (it needs the fml package). Their compiled form joins the package, and
@tt{check} checks each map against the project's structures, runs it over the examples of its
source structure and validates the results against its target; findings are placed in the map.
The structure a map reads is often not FHIR at all but a form or a message, described with
@tt{Logical:}. An @tt{Instance:} of a logical model is written like any other; name the model by
its @tt{Id}. The worked project is @tt{fhir-examples/examples/intake-maps}.

@section{In an editor, and for an AI agent}

@itemlist[
@item{@bold{In an editor.} The family's language server,
@hyperlink["https://oloveluck.github.io/fhir/fhir-lsp/index.html"]{fhir-lsp}, underlines a file's
problems as you type, using the whole project. It lists a file's entities, shows a field's
definition or a name's declaration when you hover, jumps to a definition anywhere in the project,
and completes keywords, parents and types, value sets after @tt{from}, rule sets, invariants, flags
and the fields available at a path.}
@item{@bold{For an agent.} In an
@hyperlink["https://oloveluck.github.io/fhir/fhir-mcp/index.html"]{fhir-mcp} workspace the FSH
files are one project: writing a file answers its problems at their lines, every example is
validated, and what the FSH defines is visible to the other languages by its address. The
repository's @tt{guide.md} is the language written for an agent to read (fhir-mcp serves it as
@tt{learn://fsh}; @tt{raco fhir guide fsh} prints it), @tt{ig-guide.md} is the same for the guide
file (@tt{learn://ig}), and @tt{.claude/skills/fsh} is the workflow for an agent using the command
line.}]

@section[#:tag "limits"]{Limits}

@itemlist[
@item{@bold{FHIR versions.} R4 (4.0.1) and R5 (5.0.0).}
@item{@bold{Codes.} An example's codes are tested against the code sets FHIR itself requires (a
status, a patient's gender). A binding your own profile adds, whether to a value set of the project
or of a package, is written into the profile, but @tt{check} does not test an example's code against
it. Use a terminology service for that
(@hyperlink["https://oloveluck.github.io/fhir/fhir-tx/index.html"]{fhir-tx}).}
@item{@bold{Links are not followed.} @c{Reference(Patient/ann)} is not checked to lead to a record
that exists.}
@item{@bold{Packages are not downloaded.} A dependency must already be on the machine
(@secref["dependencies"]).}
@item{@bold{The website of a guide is not built.} The pages, menus and the readable summary inside
each resource (its narrative) are the work of HL7's IG Publisher. The command line does not write
the ImplementationGuide resource either; @racketmodname[fsh/ig] and a fhir-mcp workspace do.}
@item{@bold{Snapshots are on request} (@litchar{--snapshot}); the default output is the
differential.}
@item{@bold{Not everything SUSHI writes is reproduced.} For the published guides this is tested on,
most resources come out identical to the published ones and a minority differ in details
(@secref["tested"]). An R5-only resource used in an R4 project is accepted and left as it is; the
IG Publisher would convert it.}
@item{@bold{The decompiler writes plain FSH.} It does not gather repeated addresses into aliases or
repeated rules into rule sets, and it writes a profile's fixed values as @tt{^} rules.}
@item{@bold{Building a module is not checking it.} Requiring a @tt{#lang fsh} file compiles it and
catches a misspelt field; only @tt{raco fsh check} (or @racket[check-fsh-project]) validates the
examples.}]

@section[#:tag "words"]{Words used here}

@itemlist[
@item{@bold{Resource}: one FHIR document, such as a Patient or an Observation, written as JSON.}
@item{@bold{Profile}: a set of rules a resource must follow in some setting, on top of FHIR's own.
Compiled, it is a @bold{StructureDefinition} resource.}
@item{@bold{Differential}, @bold{snapshot}: a profile written as only what it changes in its
parent, or in full.}
@item{@bold{Instance}: one record written in FSH, usually an example of a profile.}
@item{@bold{Entity}, @bold{rule}: an entity is one thing a file defines (a profile, an instance, a
value set); a rule is one of its @tt{*} lines.}
@item{@bold{Path}: the name of a field inside a record, such as @tt{name.family} or
@tt{component[systolic].code}.}
@item{@bold{Cardinality}: how many times a field may appear, written @tt{min..max}; @tt{*} means
any number.}
@item{@bold{Code system}, @bold{value set}, @bold{binding}: a catalogue of codes (LOINC, SNOMED CT);
a selection of codes; the rule tying a field to a value set.}
@item{@bold{Slice}: a named entry of a list, recognised by one of its fields (the
@italic{discriminator}).}
@item{@bold{Extension}: a field added to FHIR, identified by its address.}
@item{@bold{Invariant}: a rule stated as a FHIRPath test; @bold{FHIRPath} is FHIR's small language
for pointing inside a resource.}
@item{@bold{Canonical url}: the web address that names a definition. It is a name; nothing needs
to be served there.}
@item{@bold{Implementation Guide} (IG): profiles, value sets and examples published together for
one purpose. @bold{ImplementationGuide} is the FHIR resource that describes one.}
@item{@bold{Package}: the @tt{.tgz} file a guide is distributed as.}
@item{@bold{SUSHI}: HL7's compiler for FSH, the reference for the language.
@bold{sushi-config.yaml} is the guide file it reads. @bold{GoFSH} is HL7's tool for the other
direction, FHIR to FSH. The @bold{IG Publisher} is HL7's tool that turns SUSHI's output into a
guide's website and package.}
@item{@bold{Logical model}: a description of a data structure that is not a FHIR resource.}
@item{@bold{UCUM}: the standard for writing units (@tt{mm[Hg]}, @tt{/min}, @tt{%}).}
@item{@bold{CapabilityStatement}: the FHIR resource that lists what a server does or must do.}]

@section[#:tag "tested"]{How it is tested}

Against published guides, not only its own examples. A guide's FSH sources are compiled here and
compared, resource by resource, with the package HL7 published from the same sources (built with
SUSHI and the IG Publisher), leaving aside what only the Publisher adds.

@itemlist[
@item{Five guides compile from their own FSH with no errors. Of the resources that can be paired
with the published ones: International Patient Summary 2.0.1, 94 of 114 identical and 108 with all
the published content; mCODE 4.0.0, 311 of 349 and 339; Da Vinci Plan-Net 1.2.0, 84 of 109 and 99;
Da Vinci HRex 1.1.0, 31 of 46 and 46; Genomics Reporting 2.0.0, 121 of 141 and 129.}
@item{The other direction: the published IPS and mCODE packages decompiled to FSH and compiled
back. All 117 IPS resources come back identical, and 341 of mCODE's 349.}
@item{The parser's verdicts against SUSHI's own test suite of 515 FSH texts: 514 agree, with no
error missed and none reported where SUSHI reports none.}
@item{SUSHI's 22 hand-written pairs of FSH and JSON all compile to their JSON and survive the round
trip.}
@item{mCODE's examples, compiled and packed here, all validate against the profiles compiled here,
as they do against the published package.}]

The repository's @tt{TESTING.md} says what the remaining differences are and how to run the tests.
To work on the compiler itself, install the packages from source with the repositories side by
side, then run everything:

@verbatim{
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath ../vcl ../fhir/fhir-tx ../fhir/fhir-validation .
raco test .
}

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{fsh}}
@defmodulelang[fsh]

A file in this language provides @racket[document] (its entity ASTs), @racket[compiled] (its FHIR
resources as jsexprs), @racket[resources] (the same as typed nodes) and
@racket[(canonical-of name-or-id)] (a definition's url; an unknown name is an error that suggests
the nearest). It is compiled with the nearest @tt{sushi-config.yaml} (beside the file or up to two
levels up: canonical, FHIR version, dependencies), else for R5 under @tt{http://example.org}. A
rule-level error (a path the base type lacks) or an entity that fails to compile stops the build at
its line. Instantiating the module registers its profiles, value sets and code systems by canonical.
Run as a program, it prints the resources as indented JSON.

@subsection[#:style 'unnumbered]{@tt{fsh/compile}}
@defmodule[fsh/compile]

@defproc[(fsh->fhir [src string?]) (listof hash?)]{FSH source to its FHIR resources. A FHIR
version's types must be loaded (@racketmodname[fhir/r5/schemas]).}
@defproc[(compile-doc [entities (listof struct?)]) (listof hash?)]{
A parsed document: aliases resolved, rule sets expanded, each entity compiled. Raises on the first
entity that fails.}
@defproc[(compile-doc/errors [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)))]{
A failing entity is reported (as @racket[entity-label] and the exception) and the rest of the
document still compiles.}
@defproc[(compile-doc/diagnostics [entities (listof struct?)]) (values (listof hash?) (listof (cons/c string? exn?)) list?)]{
Also every rule-level diagnostic (@tt{fhir/diagnostic}, positioned at the rule), such as an element
path whose first step the base type does not have, or a binding to a value set nothing defines (a
warning). Compilation continues past them.}
@defproc[(compile-entity [e struct?] [aliases hash? (hash)] [profiles hash? (hash)] [invariants hash? (hash)]) (or/c hash? #f)]
@defproc[(entity-label [e struct?]) string?]{@racket["profile: Name"].}
@defproc[(instance-id [instance struct?]) string?]{An Instance entity's id: its @tt{Id:}, else an
@c{* id = "…"} rule, else its name. The one definition compiling, checking and packing share.}
@defproc[(rule-path-of [rule any/c]) (or/c string? #f)]{The element path a profile rule constrains.}
@defparam[fsh-fhir-version version string?]{The FHIR version stamped on StructureDefinitions, @racket["5.0.0"] by default; @racket["4.0.1"] with the R4 tower registered compiles an R4 IG.}
@defparam[canonical-base url string?]{@racket["http://example.org"] by default.}
@defparam[emit-snapshot? on? boolean?]{Also generate snapshots (needs the core package). Off by default.}

@subsection[#:style 'unnumbered]{@tt{fsh/parser/parse}}
@defmodule[fsh/parser/parse]

@defproc[(parse-fsh-doc [src string?] [#:source source (or/c string? path? #f) #f]) list?]{
A document's entity ASTs. A failure raises @racket[exn:fail:diagnostic] positioned in @racket[source]:
@c{t.fsh:3:18: fsh parse error: unexpected caret sequence `^short`}.}
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
bindings, @tt{only}, @tt{obeys}, concepts, includes, slicing; in an instance a code, a Coding, a
CodeableConcept of one coding, a UCUM Quantity and a plain reference as one rule each, when that rule
compiles back to exactly the JSON) and a lossless @tt{^caret} for the rest; invariants re-synthesised
from the SD constraints. Reads element types from the registered tower.}
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
                          [#:snapshot? snapshot? boolean? #f]
                          [#:texts texts (hash/c string? string?) (hash)]
                          [#:packages packages (listof path-string?) '()]
                          [#:context context (listof path-string?) '()]
                          [#:files files (or/c (listof path-string?) #f) #f]) project?]{
A file or a directory. The arguments override sushi-config.yaml; a version starting with 4 registers
the R4 tower. @racket[texts] gives a file's text in place of what is on disk (an editor's unsaved
buffer); @racket[packages] go into the definition context after the config's dependencies;
@racket[context] names other folders whose definitions are in context, their instances and their
problems left out; @racket[files] gives the project's @tt{.fsh} and map files in place of every one
under the directory. Never
exits: everything wrong is in @racket[project-diagnostics] — parse errors, a failing entity at its
declaration, a rule whose path the base type lacks, an @tt{insert} of a RuleSet the project does not
define, a @tt{Parent} or @tt{InstanceOf} nothing defines, and two definitions of one kind (or two
instances of one type) sharing an id.}
@defproc[(pack-project! [p project?] [out path-string?]) path?]{The FHIR NPM package (needs the config).}
@defproc[(find-config [src path-string?]) (or/c path? #f)]{sushi-config.yaml beside the sources or up to two levels up.}
@defproc[(fsh-files [src path-string?]) (listof path?)]{A file, or every @tt{.fsh} under a directory.}
@defproc[(map-files [src path-string?]) (listof path?)]{Every @tt{.map} or @tt{.fml} under a directory.}

@subsection[#:style 'unnumbered]{@tt{fsh/ig-config}}
@defmodule[fsh/ig-config]

The reading of @tt{sushi-config.yaml}. It lives in the foundation (@tt{fhir/ig-config}), since the
workspace reads the guide file too, and is re-exported here.

@defstruct*[ig-config ([canonical any/c] [fhir-version any/c] [dependencies list?] [id any/c] [name any/c] [title any/c]
                       [version any/c] [status any/c] [license any/c] [description any/c] [publisher any/c]
                       [yaml any/c])]{
What compiling and packaging read from sushi-config.yaml; any field may be @racket[#f]. A dependency
is @racket[(cons package-id version)]; @racket[yaml] is the whole file as read.}
@defproc[(read-ig-config [file path-string?]) ig-config?]
@defproc[(parse-ig-config [text string?]) ig-config?]{The same from the file's text.}
@deftogether[(
@defproc[(ig-config-depends-on [c ig-config?]) list?]
@defproc[(ig-config-globals [c ig-config?]) (listof (cons/c string? string?))]
@defproc[(ig-config-experimental [c ig-config?]) (or/c boolean? 'unstated)]
@defproc[(ig-config-fhir-versions [c ig-config?]) (listof string?)]
@defproc[(ig-config-expansion-parameters [c ig-config?]) (or/c string? #f)])]{
What the struct's fields do not hold, read from the file: the dependencies with their @tt{id} and
@tt{uri}; the @tt{global} profiles as @racket[(cons type profile)]; the @tt{experimental} flag;
every FHIR version listed; and the id of the Parameters resource that
@tt{parameters: path-expansion-params: …/Parameters-<id>.json} names (the guide's pinned versions).}
@defproc[(dependency-packages [deps (listof (cons/c string? string?))]) (values (listof path?) (listof (cons/c string? string?)))]{
Each dependency's package as @tt{<id>-<version>.tgz}, in @tt{fhir-spec} or the package directories,
and the ones missing.}
@defproc[(implicit-packages [fhir-version (or/c string? #f)]) (listof path?)]{
The packages SUSHI loads without being asked — the newest @tt{hl7.fhir.uv.extensions} and
@tt{hl7.terminology} for the version — when a copy is at hand, else none.}

@subsection[#:style 'unnumbered]{@tt{fsh/ig}}
@defmodule[fsh/ig]

The guide file as a language: @tt{sushi-config.yaml} compiled to the ImplementationGuide resource,
and back. The workspace support that uses it is @tt{fsh/ig-workspace} (language id @tt{ig}; its
@tt{guide} is @filepath{ig-guide.md}, fhir-mcp's @tt{learn://ig}).

@defproc[(implementation-guide [config ig-config?] [resources (listof hash?)]
                               [#:fhir-version fhir-version (or/c string? #f) #f]
                               [#:canonical canonical (or/c string? #f) #f]
                               [#:labels labels (hash/c string? list?) (hash)]) hash?]{
The ImplementationGuide: the file's id, url (@tt{<canonical>/ImplementationGuide/<id>}), version,
name, title, status (draft when absent), publisher, license, FHIR version, dependencies and
@tt{global} profiles, and one @tt{definition.resource} per resource given. A resource with a
@tt{url} is listed as a definition; any other as an example, of the first profile it claims that
this guide defines. @racket[labels] (from @racket[instance-labels]) names and describes examples by
their FSH @tt{Title:} and @tt{Description:}. The two keywords are used where the file gives no FHIR
version or canonical. Pages, menus and templates are not written.}
@defproc[(implementation-guide->config-text [guide hash?]) string?]{
An ImplementationGuide as the @tt{sushi-config.yaml} that states it: compiling the text gives the
guide's header, dependencies, globals and parameters back. What the guide lists is not written; it
is derived from the project.}
@defproc[(ig-diagnostics [text string?] [file path-string?]
                         [#:defined? defined? (-> string? any/c) (lambda (url) #t)]
                         [#:resource-types resource-types (or/c #f (listof string?)) #f]) (listof diagnostic?)]{
The file's problems, each at its key's line: no @tt{id}, no @tt{canonical}, no @tt{version} (a
warning), a status that is none of FHIR's, a dependency no package at hand provides (a warning), a
@tt{global} profile nothing defines (decided by @racket[defined?]) or for a type that is not one of
@racket[resource-types].}
@defproc[(instance-labels [entities list?]) (hash/c string? list?)]{What a document's Instances say
of themselves: per id, a @racket[(cons title description)] for each instance with it, in document
order.}
@defproc[(key-line [text string?] [key string?] [#:under parent (or/c string? #f) #f])
         (or/c (cons/c exact-positive-integer? exact-positive-integer?) #f)]{
The line and column where a top-level key, or a key in the block under a top-level one, is written.}

@subsection[#:style 'unnumbered]{@tt{fsh/pack}}
@defmodule[fsh/pack]

@defproc[(pack-package! [out path-string?] [config ig-config?] [resources (listof hash?)] [usages (hash/c string? string?)]) path?]{
Writes the package: @tt{package.json} from the config, conformance resources and @tt{#definition}
instances in @tt{package/}, other instances in @tt{package/example/}, an index per folder.
@racket[usages] maps an instance id to @racket["#example"] or @racket["#definition"].}
@defproc[(conformance-type? [type string?]) boolean?]

@subsection[#:style 'unnumbered]{@tt{fsh/check}}
@defmodule[fsh/check]

@defproc[(check-fsh-project [dir path-string?] [#:capability capability (or/c string? #f) #f]
                            [#:canonical canonical (or/c string? #f) #f]
                            [#:fhir-version fhir-version (or/c string? #f) #f]
                            [#:packages packages (listof path-string?) '()]
                            [#:profiles profiles (hash/c string? (listof string?)) (hash)]
                            [#:context context (listof path-string?) '()]
                            [#:files files (or/c (listof path-string?) #f) #f]) check-report?]{
The project end to end: compile (the keywords as for @racket[compile-project]); validate every
example against its base type and the profiles it claims, in the same package context (the project's
own artifacts ahead of its dependencies and core), and against the profiles @racket[profiles] names
for its type (resource type to canonicals: a workspace's constraints); with a CapabilityStatement id,
the generated server's unmet
requirements (loads @tt{fhir-server} on demand; needs the project's sushi-config.yaml, since the
server is generated from the project packed as a package).
Every problem is a @tt{fhir/diagnostic} positioned in the FSH: a validation issue is traced to the
instance rule that wrote the element and names the profile rule that imposes the constraint broken.
The project's maps are
checked against its structures in the same context (@tt{fml/project}, on demand), their findings
positioned in the map.}
@defstruct*[check-report ([diagnostics list?] [texts hash?] [examples exact-nonnegative-integer?]
                          [invalid exact-nonnegative-integer?] [artifacts list?])]
@defproc[(trace-rule [rules list?] [issue-path string?]
                     [#:exact? exact? any/c #f] [#:slice slice (or/c string? #f) #f]) (or/c any/c #f)]{
The rule whose path best matches a validator path: the type root dropped, brackets and choice suffixes
normalised, the longest leading run of steps wins. @racket[exact?] (for profile rules) wants the rule
to name the element itself or its parent; @racket[slice] prefers the rules that address that slice.}

@subsection[#:style 'unnumbered]{@tt{fsh/support}}
@defmodule[fsh/support]

@defthing[support lang-support?]{
FSH's @tt{fhir/lang/support} value, which the language server (fhir-lsp) loads by collection name:
a document's diagnostics with its project, the project's files, the outline, hover (an element path
in the base type; a name's declaration), go to definition across the project, and completion
(keywords, Parent and InstanceOf types and profiles, value sets after @tt{from}, rule sets,
invariants, the base type's elements at a path, flags). Declarations are read from the text, so it
works while a document does not parse yet.}

@subsection[#:style 'unnumbered]{@tt{fsh/workspace}}
@defmodule[fsh/workspace]

@defthing[workspace-support ws-support?]{
FSH's @tt{fhir/lang/workspace} support, which fhir-mcp loads by collection name: the workspace's
@tt{.fsh} files are one project, checked with the workspace's canonical, FHIR version, packages and
profile constraints (@racket[check-fsh-project]); @tt{run} of a file gives the resources its entities
compile to, each with its problems; @tt{explain} of an entity gives its resource and its problems, an
example's validation issues traced to the rules.}
@defthing[guide string?]{The text of @filepath{guide.md}, the language guide for agents (fhir-mcp's
@tt{learn://fsh}).}
