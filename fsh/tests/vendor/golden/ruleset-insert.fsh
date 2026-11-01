RuleSet: NameRules
* name 1..*
* name.family 1..1
* name.given 1..*

Profile: InsertPatient
Parent: Patient
Id: insert-patient
Title: "Insert Patient"
* insert NameRules
* gender 1..1
