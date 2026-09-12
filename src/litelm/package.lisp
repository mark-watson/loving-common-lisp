;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License

(defpackage #:litelm
  (:use #:cl)
  (:export
   ;; main entry points
   #:completion
   #:embedding
   ;; provider registry / routing
   #:define-provider
   #:find-provider
   #:parse-model
   ;; response accessors
   #:response
   #:response-content
   #:response-tool-calls
   #:response-finish-reason
   #:response-model
   #:response-usage
   #:response-raw
   ;; condition hierarchy
   #:litelm-error
   #:api-error
   #:api-error-status
   #:api-error-body
   #:authentication-error
   #:rate-limit-error
   #:not-found-error
   #:context-window-exceeded-error
   ;; json utilities (exported for tests and advanced use)
   #:json-encode
   #:json-decode))
