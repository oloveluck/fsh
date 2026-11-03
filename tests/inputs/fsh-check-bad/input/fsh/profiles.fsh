Profile: TumorSize
Parent: Observation
Id: tumor-size
* subject 1..1
* component ^slicing.discriminator.type = #pattern
* component ^slicing.discriminator.path = "code"
* component ^slicing.rules = #open
* component contains longest 1..1
* component[longest].code = http://loinc.org#33728-7
* component[longest].value[x] only Quantity
