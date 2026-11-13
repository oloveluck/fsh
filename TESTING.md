# Testing fsh

The real Implementation Guides are the measure: each one's own FSH, compiled here, is compared with
the package HL7 published from it, resource by resource, after normalising what only the IG Publisher
adds (narratives, snapshots, dates, OIDs). Floors only rise.

## Results

**Compiling the IGs' own FSH**

| IG | Compile errors | Resources paired | Identical | With all published content | Differential elements |
|---|---|---|---|---|---|
| International Patient Summary 2.0.1 | 0 | 114 | 108 | 111 | 438/438 |
| mCODE 4.0.0 (with US Core 6.1.0 and Genomics Reporting 2.0.0) | 0 | 349 | 338 | 347 | 472/473 |
| Da Vinci Plan-Net 1.2.0 | 0 | 109 | 104 | 105 | 321/321 |
| Da Vinci HRex 1.1.0 | 0 | 46 | 46 | 46 | 138/138 |
| Genomics Reporting 2.0.0 | 0 | 141 | 134 | 137 | 254/259 |

**Decompiling the published packages** (FHIR → FSH → FHIR, compared the same way)

| Package | Paired | Identical | With all published content |
|---|---|---|---|
| IPS 2.0.1 | 117 | 117 | 117 |
| mCODE 4.0.0 | 349 | 348 | 348 |

**The parser's diagnostics against SUSHI's own importer tests** (515 FSH texts from SUSHI 3.21.0,
111 with the error or warning SUSHI logs): 514 of 515 verdicts agree, 95 of them on SUSHI's own line;
no missed errors, no over-reports, no crashes. The one line elsewhere is an empty RuleSet, whose error
SUSHI positions at the next entity.

**The SUSHI golden corpus** (hand-authored FSH and JSON pairs): 22/22 compile to their JSON, and 22/22
survive FSH → FHIR → FSH → FHIR unchanged.

**The documentation.** `tests/guide-test.rkt`: every FSH block in guide.md is a project that checks
with no error and has an example to validate; the worked example's failure and each common problem
are reported as the guide quotes them. `tests/docs-test.rkt`: scribblings/fsh.scrbl renders with no
broken reference to its own sections or `fsh/…` bindings.

## What the remaining differences are

`tests/ig-harness.rkt` classifies every difference by kind, largest first, and the tests print the
classes: what the published resource has and ours lacks or has otherwise (`missing`, `differs`), and
what ours writes that the published one does not (`surplus`, `extra`). What is left: a quantity's
`unit` (written here from the UCUM code when the rule gives no display; SUSHI writes none — about
twenty examples across the guides); four Plan-Net profiles with an element SUSHI leaves out; an empty `^comment = ""` (two mCODE profiles); IPS's Bundle narratives; one mCODE
example whose published JSON is not what its FSH says; and in Genomics Reporting the SUSHI 2 shape
of a type-specific choice path, which SUSHI 3 and this compiler write as a type slice, and
OperationDefinitions that changed after publication. `CLAUDE.md` lists them in full.

## Running them

```sh
raco test .                              # everything, with the floors
raco test tests/ig-ips-test.rkt          # one IG (also ig-mcode, ig-genomics, gofsh-ips, gofsh-mcode)
raco test tests/sushi-importer-test.rkt  # the importer oracle
raco test tests/golden-test.rkt tests/roundtrip-test.rkt
raco test tests/guide-test.rkt tests/docs-test.rkt   # the guide and the manual
raco test tests/sushi-alignment-test.rkt tests/cli-test.rkt   # SUSHI's behaviours, case by case; the command line
```

`tests/ig-plannet-test.rkt` and `tests/ig-hrex-test.rkt` fetch their packages from
packages.fhir.org once (checked against pinned checksums, kept in `tests/vendor/packages/`) and skip
with a note when offline.
