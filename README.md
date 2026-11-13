# fsh — FHIR Shorthand

[FHIR Shorthand](https://hl7.org/fhir/uv/shorthand/) (FSH) is the language people write FHIR
*profiles* in: the rules that say what valid health data looks like in one setting, with the code
sets they use and examples of the data. This package compiles FSH to the FHIR resources it stands
for, checks every example against the profiles and points at the line that breaks a rule, and turns
FHIR JSON back into FSH. It reads the same files as SUSHI, HL7's reference compiler.

```fsh
Profile: LabResult
Parent: Observation
* status = #final
* subject 1..1

Instance: ann-hba1c
InstanceOf: LabResult
* status = #preliminary
* code = http://loinc.org#4548-4
* subject = Reference(Patient/ann)
```

```
$ fsh check clinic/
clinic/lab.fsh:8:3: fsh validate error: instance ann-hba1c: Observation.status: value does not match required pattern "final" (constrained by profile LabResult at line 3)
 8 | * status = #preliminary
   |   ^^^^^^
fsh check: 1 example, 1 with errors; 1 error, 0 warnings
```

## Install

Build the [fhir family's tools](https://github.com/oloveluck/fhir#install) once; `fsh` is one of
them. With Racket, `raco fsh` is the same command.

## Documentation

**[oloveluck.github.io/fsh](https://oloveluck.github.io/fsh/)**: a first example explained line by
line, writing profiles and examples, projects and packages, and the reference.

- How well it works (five published guides compiled and compared with HL7's): [TESTING.md](TESTING.md).
- For AI agents: [guide.md](guide.md) and [ig-guide.md](ig-guide.md), cheat sheets with tested
  examples, and the [fsh skill](.claude/skills/fsh/SKILL.md).
- How it is built: [CLAUDE.md](CLAUDE.md).

Part of the [fhir](https://github.com/oloveluck/fhir) family. License: MIT.
