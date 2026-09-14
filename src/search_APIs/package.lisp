;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License

(defpackage #:search-apis
  (:nicknames #:search_apis)
  (:use #:cl)
  (:export
   ;; main entry point
   #:websearch
   ;; result and response structures
   #:search-result
   #:search-result-title
   #:search-result-url
   #:search-result-content
   #:search-result-score
   #:search-response
   #:search-response-provider
   #:search-response-query
   #:search-response-answer
   #:search-response-results
   #:search-response-raw
   ;; provider registry
   #:search-provider
   #:search-provider-name
   #:search-provider-base-url
   #:search-provider-env-keys
   #:define-search-provider
   #:find-search-provider
   ;; condition hierarchy
   #:search-error
   #:api-error
   #:api-error-status
   #:api-error-body
   #:authentication-error
   #:rate-limit-error
   #:not-found-error
   ;; json utilities (exported for tests and advanced use)
   #:json-encode
   #:json-decode))
