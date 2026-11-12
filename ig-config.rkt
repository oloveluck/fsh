#lang racket/base

;;; ig-config.rkt — the reading of sushi-config.yaml is the foundation's (fhir/ig-config): the
;;; workspace reads the guide file too. Re-exported here for fsh's own modules and its users.

(require fhir/ig-config)
(provide (all-from-out fhir/ig-config))
