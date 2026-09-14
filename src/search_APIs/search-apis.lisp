;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Common web search core: the shared result/response format, the provider
;;; registry, HTTP helpers, and the WEBSEARCH entry point. Provider-specific
;;; request and response handling lives in providers.lisp.

(in-package #:search-apis)

;;; ---- conditions (mirrors the litelm exception hierarchy) ----

(define-condition search-error (simple-error) ())

(define-condition api-error (search-error)
  ((status :initarg :status :reader api-error-status)
   (body :initarg :body :reader api-error-body))
  (:report (lambda (c stream)
             (format stream "Web search API error ~A: ~A"
                     (api-error-status c) (api-error-body c)))))

(define-condition authentication-error (api-error) ())
(define-condition rate-limit-error (api-error) ())
(define-condition not-found-error (api-error) ())

(defun %map-http-error (status body)
  "Map an HTTP status code to the corresponding search-apis condition."
  (let ((condition
          (cond ((member status '(401 403)) 'authentication-error)
                ((= status 429) 'rate-limit-error)
                ((= status 404) 'not-found-error)
                (t 'api-error))))
    (error condition :status status :body body)))

;;; ---- result and response ----

(defstruct search-result
  "One web search hit. CONTENT is the provider's snippet/summary and may be
nil when the provider only returns links. SCORE is provider specific."
  title
  url
  content
  (score nil))

(defstruct search-response
  "The result of a WEBSEARCH call. ANSWER is populated by search-plus-LLM
providers such as Perplexity; RESULTS is the list of SEARCH-RESULTs."
  provider
  query
  answer
  results
  raw)

;;; ---- provider registry ----

(defstruct search-provider
  name          ; keyword, e.g. :brave
  base-url      ; endpoint URL
  env-keys      ; list of environment variables tried in order for the API key
  (requires-key t)
  function)     ; (lambda (provider query &key ...) -> search-response)

(defvar *search-providers* (make-hash-table :test 'eq))

(defun define-search-provider (name base-url &key env-keys (requires-key t) function)
  "Register a search provider. NAME is a keyword, BASE-URL the endpoint,
ENV-KEYS a list of environment variables tried in order for the API key, and
FUNCTION the provider's search implementation."
  (setf (gethash name *search-providers*)
        (make-search-provider
         :name name
         :base-url base-url
         :env-keys (if (listp env-keys) env-keys (list env-keys))
         :requires-key requires-key
         :function function)))

(defun find-search-provider (name)
  (or (gethash name *search-providers*)
      (error 'search-error
             :format-control "Unknown search provider ~S. Known providers: ~S"
             :format-arguments
             (list name (loop for k being the hash-keys of *search-providers*
                              collect k)))))

(defun provider-api-key (provider explicit-key)
  (or explicit-key
      (loop for var in (search-provider-env-keys provider)
            for value = (uiop:getenv var)
            when (and value (plusp (length value))) return value)
      (if (search-provider-requires-key provider)
          (error 'search-error
                 :format-control "No API key for provider ~S. Pass :api-key or set one of ~S"
                 :format-arguments (list (search-provider-name provider)
                                         (search-provider-env-keys provider)))
          nil)))

;;; ---- HTTP ----

(defun %get-json (url headers)
  "GET URL and decode the JSON body into a Lisp alist."
  (handler-case
      (json-decode (dex:get url :headers headers))
    (dex:http-request-failed (e)
      (%map-http-error (dex:response-status e) (dex:response-body e)))))

(defun %post-json (url headers payload)
  "POST PAYLOAD (a nested alist) as JSON to URL and decode the response."
  (handler-case
      (json-decode (dex:post url :headers headers :content (json-encode payload)))
    (dex:http-request-failed (e)
      (%map-http-error (dex:response-status e) (dex:response-body e)))))

;;; ---- main entry point ----

(defun websearch (query &key (provider :brave) api-key max-results model)
  "Search the web for QUERY using PROVIDER, a keyword such as :brave, :tavily
or :perplexity. MAX-RESULTS is a hint (used by Brave and Tavily). MODEL selects
the search-plus-LLM model for Perplexity (default \"sonar-pro\").

Returns a SEARCH-RESPONSE. Use SEARCH-RESPONSE-RESULTS for the list of hits and
SEARCH-RESPONSE-ANSWER for providers that also synthesize an answer. The API key
comes from API-KEY or, when nil, from the provider's environment variables."
  (let* ((p (find-search-provider provider))
         (key (provider-api-key p api-key)))
    (funcall (search-provider-function p) p query
             :api-key key
             :max-results (or max-results 5)
             :model model)))
