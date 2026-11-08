# Testing fsh

The real Implementation Guides are the measure: each one's own FSH, compiled here, is compared with
the package HL7 published from it, resource by resource, after normalising what only the IG Publisher
adds (narratives, snapshots, dates, OIDs). Floors only rise.

## Results

**Compiling the IGs' own FSH**

| IG | Compile errors | Resources paired | Identical | With all published content | Differential elements |
|---|---|---|---|---|---|
| International Patient Summary 2.0.1 | 0 | 114 | 94 | 108 | 438/438 |
| mCODE 4.0.0 (with US Core 6.1.0 and Genomics Reporting 2.0.0) | 0 | 349 | 311 | 339 | 472/473 |
| Da Vinci Plan-Net 1.2.0 | 0 | 109 | 84 | 99 | 321/321 |
| Da Vinci HRex 1.1.0 | 0 | 46 | 31 | 46 | 138/138 |
| Genomics Reporting 2.0.0 | 0 | 141 | 113 | 129 | 254/259 |

**Decompiling the published packages** (FHIR → FSH → FHIR, compared the same way)

| Package | Paired | Identical | With all published content |
|---|---|---|---|
| IPS 2.0.1 | 117 | 117 | 117 |
| mCODE 4.0.0 | 349 | 341 | 345 |

**The parser's diagnostics against SUSHI's own importer tests** (515 FSH texts from SUSHI 3.21.0,
111 with the error or warning SUSHI logs): 514 of 515 verdicts agree, 95 of them on SUSHI's own line;
no missed errors, no over-reports, no crashes. The one line elsewhere is an empty RuleSet, whose error
SUSHI positions at the next entity.

**The SUSHI golden corpus** (hand-authored FSH and JSON pairs): 22/22 compile to their JSON, and 22/22
survive FSH → FHIR → FSH → FHIR unchanged.

## What the remaining differences are

`tests/ig-harness.rkt` classifies every difference by kind, largest first, and the tests print the
classes. The largest: profiles whose differential lacks elements SUSHI writes (mostly mCODE's genomics
and performance-status profiles), a few instance details, Bundle narratives, and in Genomics
Reporting the SUSHI 2 shape of a type-specific choice path, which SUSHI 3 and this compiler write as
a type slice. `CLAUDE.md` lists them in full.

## Running them

```sh
raco test .                              # everything, with the floors
raco test tests/ig-ips-test.rkt          # one IG (also ig-mcode, ig-genomics, gofsh-ips, gofsh-mcode)
raco test tests/sushi-importer-test.rkt  # the importer oracle
raco test tests/golden-test.rkt tests/roundtrip-test.rkt
```

`tests/ig-plannet-test.rkt` and `tests/ig-hrex-test.rkt` fetch their packages from
packages.fhir.org once (checked against pinned checksums, kept in `tests/vendor/packages/`) and skip
with a note when offline.
