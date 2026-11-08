3878
((3) 0 () 11 ((q lib "fsh/check.rkt") (q 6370 . 11) (q lib "fsh/project.rkt") (q lib "fsh/compile.rkt") (q lib "fsh/pack.rkt") (q 3754 . 21) (q lib "fsh/ig-config.rkt") (q 5043 . 23) (q lib "fsh/decompile.rkt") (q lib "fsh/parser/parse.rkt") (q lib "fsh/print.rkt")) () (h ! (equal) ((c def c (c (? . 4) q pack-package!)) q (5898 . 6)) ((c def c (c (? . 6) q ig-config-description)) c (? . 7)) ((c def c (c (? . 0) q check-report-invalid)) c (? . 1)) ((c def c (c (? . 9) q parse-fsh-doc)) q (2679 . 4)) ((q def ((lib "fsh/lint.rkt") lint-document)) q (3248 . 3)) ((c def c (c (? . 3) q entity-label)) q (2252 . 3)) ((c def c (c (? . 0) q struct:check-report)) c (? . 1)) ((c def c (c (? . 2) q project)) c (? . 5)) ((c def c (c (? . 2) q compile-project)) q (4334 . 9)) ((c def c (c (? . 8) q node->fsh)) q (3616 . 3)) ((c def c (c (? . 3) q compile-doc/diagnostics)) q (1793 . 6)) ((c def c (c (? . 4) q conformance-type?)) q (6094 . 3)) ((c def c (c (? . 10) q print-value)) q (3487 . 3)) ((c def c (c (? . 2) q project-maps)) c (? . 5)) ((c def c (c (? . 6) q ig-config-license)) c (? . 7)) ((c def c (c (? . 0) q check-fsh-project)) q (6165 . 5)) ((c def c (c (? . 6) q read-ig-config)) q (5647 . 3)) ((c def c (c (? . 3) q compile-doc/errors)) q (1629 . 4)) ((c def c (c (? . 3) q emit-snapshot?)) q (2584 . 4)) ((c def c (c (? . 6) q dependency-packages)) q (5722 . 4)) ((c def c (c (? . 2) q project-fhir-version)) c (? . 5)) ((c def c (c (? . 6) q ig-config-name)) c (? . 7)) ((c def c (c (? . 6) q ig-config-canonical)) c (? . 7)) ((c def c (c (? . 0) q check-report-artifacts)) c (? . 1)) ((c def c (c (? . 2) q project-located)) c (? . 5)) ((c def c (c (? . 2) q project-sd-packages)) c (? . 5)) ((c def c (c (? . 3) q compile-doc)) q (1539 . 3)) ((c def c (c (? . 6) q ig-config-version)) c (? . 7)) ((c def c (c (? . 2) q pack-project!)) q (4731 . 4)) ((c def c (c (? . 8) q fhir->fsh)) q (3543 . 3)) ((c def c (c (? . 6) q ig-config-id)) c (? . 7)) ((c def c (c (? . 2) q project-texts)) c (? . 5)) ((c def c (c (? . 9) q parse-fsh-doc/diagnostics)) q (2959 . 6)) ((c def c (c (? . 10) q print-doc)) q (3362 . 3)) ((c def c (c (? . 9) q parse-fsh-entity)) q (3183 . 3)) ((c def c (c (? . 2) q map-files)) q (4970 . 3)) ((c def c (c (? . 3) q fsh->fhir)) q (1471 . 3)) ((c def c (c (? . 10) q print-entity)) q (3430 . 3)) ((c def c (c (? . 6) q ig-config)) c (? . 7)) ((c def c (c (? . 9) q parse-fsh-doc/located)) q (2810 . 4)) ((c def c (c (? . 6) q ig-config?)) c (? . 7)) ((c def c (c (? . 6) q ig-config-dependencies)) c (? . 7)) ((c def c (c (? . 2) q project-missing)) c (? . 5)) ((c def c (c (? . 8) q fhir->fsh-entity)) q (3681 . 3)) ((c def c (c (? . 3) q canonical-base)) q (2491 . 4)) ((c def c (c (? . 0) q check-report-examples)) c (? . 1)) ((c def c (c (? . 3) q compile-entity)) q (1944 . 9)) ((c def c (c (? . 0) q check-report-diagnostics)) c (? . 1)) ((c def c (c (? . 2) q project-files)) c (? . 5)) ((c def c (c (? . 6) q struct:ig-config)) c (? . 7)) ((c def c (c (? . 2) q project-config)) c (? . 5)) ((c def c (c (? . 2) q find-config)) q (4820 . 3)) ((c def c (c (? . 2) q fsh-files)) q (4897 . 3)) ((c def c (c (? . 6) q ig-config-title)) c (? . 7)) ((c def c (c (? . 0) q trace-rule)) q (6709 . 4)) ((c def c (c (? . 2) q project-artifacts)) c (? . 5)) ((c def c (c (? . 2) q struct:project)) c (? . 5)) ((c def c (c (? . 6) q ig-config-fhir-version)) c (? . 7)) ((c def c (c (? . 0) q check-report?)) c (? . 1)) ((c def c (c (? . 2) q project-diagnostics)) c (? . 5)) ((c def c (c (? . 2) q project?)) c (? . 5)) ((c def c (c (? . 3) q fsh-fhir-version)) q (2386 . 4)) ((c def c (c (? . 0) q check-report-texts)) c (? . 1)) ((c def c (c (? . 6) q ig-config-status)) c (? . 7)) ((c def c (c (? . 0) q check-report)) c (? . 1)) ((c def c (c (? . 6) q ig-config-publisher)) c (? . 7)) ((c def c (c (? . 3) q rule-path-of)) q (2311 . 3))))

rule                                                     example
cardinality and flags                                    * subject 1..1 MS
assignment (a pattern; add (exactly) for a fixed value)  * code = $loinc#85354-9 "Blood pressure panel"
binding                                                  * category from ObservationCategoryCodes (required)
type                                                     * value[x] only Quantity or CodeableConcept
slicing                                                  * component contains systolic 1..1 and diastolic 1..1
invariant                                                * obeys bp-1
rule set                                                 * insert CommonMetadata
anything else, through the element definition            * code ^short = "What was measured"

raco fsh check ig/                   compile; validate every example against the project's profiles; problems
                                     on their FSH lines
raco fsh check ig/ –capability <id>  also what a server generated from that CapabilityStatement would not do
                                     (needs fhir-server)
raco fsh ig/                         compile every .fsh to ig/fsh-generated/<Type>-<id>.json
raco fsh ig/ –pack [out.tgz]         also write the FHIR NPM package
raco fsh file.fsh                    one file, with the default canonical and R5
raco fsh –decompile sd.json          FHIR to FSH
procedure
(fsh->fhir src) -> (listof hash?)
  src : string?
procedure
(compile-doc entities) -> (listof hash?)
  entities : (listof struct?)
procedure
(compile-doc/errors entities) -> (listof hash?)
                                 (listof (cons/c string? exn?))
  entities : (listof struct?)
procedure
(compile-doc/diagnostics entities)
 -> (listof hash?)
    (listof (cons/c string? exn?))
    list?
  entities : (listof struct?)
procedure
(compile-entity  e                
                [aliases          
                 profiles         
                 invariants]) -> (or/c hash? #f)
  e : struct?
  aliases : hash? = (hash)
  profiles : hash? = (hash)
  invariants : hash? = (hash)
procedure
(entity-label e) -> string?
  e : struct?
procedure
(rule-path-of rule) -> (or/c string? #f)
  rule : any/c
parameter
(fsh-fhir-version) -> string?
(fsh-fhir-version version) -> void?
  version : string?
parameter
(canonical-base) -> string?
(canonical-base url) -> void?
  url : string?
parameter
(emit-snapshot?) -> boolean?
(emit-snapshot? on?) -> void?
  on? : boolean?
procedure
(parse-fsh-doc src [#:source source]) -> list?
  src : string?
  source : (or/c string? path? #f) = #f
procedure
(parse-fsh-doc/located src [#:source source]) -> (listof pair?)
  src : string?
  source : (or/c string? path? #f) = #f
procedure
(parse-fsh-doc/diagnostics  src               
                           [#:source source]) 
 -> (listof pair?) (listof diagnostic?)
  src : string?
  source : (or/c string? path? #f) = #f
procedure
(parse-fsh-entity src) -> any/c
  src : string?
procedure
(lint-document items) -> (listof diagnostic?)
  items : (listof (list/c any/c any/c list?))
procedure
(print-doc entities) -> string?
  entities : list?
procedure
(print-entity e) -> string?
  e : any/c
procedure
(print-value v) -> string?
  v : any/c
procedure
(fhir->fsh resource ...) -> string?
  resource : hash?
procedure
(node->fsh node ...) -> string?
  node : fhir?
procedure
(fhir->fsh-entity resource) -> any/c
  resource : hash?
struct
(struct project (config
                 fhir-version
                 files
                 texts
                 located
                 artifacts
                 diagnostics
                 missing
                 sd-packages
                 maps))
  config : (or/c ig-config? #f)
  fhir-version : string?
  files : (listof path?)
  texts : hash?
  located : list?
  artifacts : (listof hash?)
  diagnostics : list?
  missing : list?
  sd-packages : (listof path?)
  maps : list?
procedure
(compile-project  src                             
                 [#:canonical canonical           
                  #:fhir-version fhir-version     
                  #:snapshot? snapshot?])     -> project?
  src : path-string?
  canonical : (or/c string? #f) = #f
  fhir-version : (or/c string? #f) = #f
  snapshot? : boolean? = #f
procedure
(pack-project! p out) -> path?
  p : project?
  out : path-string?
procedure
(find-config src) -> (or/c path? #f)
  src : path-string?
procedure
(fsh-files src) -> (listof path?)
  src : path-string?
procedure
(map-files src) -> (listof path?)
  src : path-string?
struct
(struct ig-config (canonical
                   fhir-version
                   dependencies
                   id
                   name
                   title
                   version
                   status
                   license
                   description
                   publisher))
  canonical : any/c
  fhir-version : any/c
  dependencies : list?
  id : any/c
  name : any/c
  title : any/c
  version : any/c
  status : any/c
  license : any/c
  description : any/c
  publisher : any/c
procedure
(read-ig-config file) -> ig-config?
  file : path-string?
procedure
(dependency-packages deps) -> (listof path?)
                              (listof (cons/c string? string?))
  deps : (listof (cons/c string? string?))
procedure
(pack-package! out config resources usages) -> path?
  out : path-string?
  config : ig-config?
  resources : (listof hash?)
  usages : (hash/c string? string?)
procedure
(conformance-type? type) -> boolean?
  type : string?
procedure
(check-fsh-project  dir                           
                   [#:capability capability]) -> check-report?
  dir : path-string?
  capability : (or/c string? #f) = #f
struct
(struct check-report (diagnostics
                      texts
                      examples
                      invalid
                      artifacts))
  diagnostics : list?
  texts : hash?
  examples : exact-nonnegative-integer?
  invalid : exact-nonnegative-integer?
  artifacts : list?
procedure
(trace-rule rules issue-path) -> (or/c any/c #f)
  rules : list?
  issue-path : string?
