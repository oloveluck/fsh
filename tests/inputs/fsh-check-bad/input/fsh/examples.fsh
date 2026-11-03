Instance: tumor-1
InstanceOf: TumorSize
* status = #final
* code = http://loinc.org#21889-1
* component[longest].valueString = "big"

Instance: tumor-2
InstanceOf: TumorSize
* status = #bogus
* code = http://loinc.org#21889-1
* subject = Reference(Patient/p1)
* component[longest].valueQuantity = 2.5 'cm'
