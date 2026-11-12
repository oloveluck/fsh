#lang racket/base

;;; yaml.rkt — the YAML reader moved to the foundation (fhir/yaml): the workspace reads
;;; sushi-config.yaml with it too. Re-exported here for fsh's own modules.

(require fhir/yaml)
(provide (all-from-out fhir/yaml))
