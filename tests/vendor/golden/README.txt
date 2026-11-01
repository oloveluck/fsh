Golden FSH -> FHIR pairs (<name>.fsh + <name>.json). The .json is the expected FHIR resource as produced by the
reference compiler SUSHI (https://github.com/FHIR/sushi), assuming an IG canonical of `http://example.org`.

The GoldenSuite compiles each .fsh with fhir4s and asserts our output is a SUBSET of the reference JSON (every field we
emit matches; reference-only fields we don't emit yet are tolerated) — report-only with a ratcheting floor.

To (re)generate authoritatively, run `sushi` offline on the .fsh inputs (IG canonical http://example.org) and copy the
emitted resource JSON here. These initial pairs are hand-authored to match SUSHI's documented output for simple cases.
