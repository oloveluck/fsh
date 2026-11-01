Profile: BloodPressure
Parent: Observation
Id: blood-pressure
Title: "Blood Pressure Profile"
* component ^slicing.discriminator[0].type = #pattern
* component ^slicing.discriminator[0].path = "code"
* component ^slicing.rules = #open
* component contains systolic 1..1 and diastolic 1..1
* component[systolic].code = http://loinc.org#8480-6
* component[diastolic].code = http://loinc.org#8462-4
