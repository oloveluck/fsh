Profile: ObeysPatient
Parent: Patient
Id: obeys-patient
Title: "Obeys Patient"
* obeys pat-1

Invariant: pat-1
Description: "Either a name or an identifier must be present"
Severity: #error
Expression: "name.exists() or identifier.exists()"
