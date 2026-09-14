;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Provider implementations for Brave, Tavily, and Perplexity. Each provider
;;; turns its own request/response shape into the shared SEARCH-RESPONSE format.
;;; The pure response parsers are separate functions so they can be tested
;;; offline against recorded JSON.

(in-package #:search-apis)

;;; ---- Brave: GET, key in the X-Subscription-Token header ----

(defun parse-brave-response (json query)
  "Turn a decoded Brave web search response into a SEARCH-RESPONSE."
  (let* ((web (aget json "web"))
         (items (and web (aget web "results"))))
    (make-search-response
     :provider :brave
     :query query
     :results (loop for item in items
                    collect (make-search-result
                             :title (aget item "title")
                             :url (aget item "url")
                             :content (aget item "description")))
     :raw json)))

(defun brave-search (provider query &key api-key (max-results 5) model &allow-other-keys)
  (declare (ignore model))
  (let* ((url (format nil "~A?q=~A&count=~D"
                      (search-provider-base-url provider)
                      (quri:url-encode query :space-to-plus t)
                      max-results))
         (headers (list (cons "Accept" "application/json")
                        (cons "X-Subscription-Token" api-key)))
         (json (%get-json url headers)))
    (parse-brave-response json query)))

;;; ---- Tavily: POST, key in the JSON body ----

(defun parse-tavily-response (json query)
  "Turn a decoded Tavily search response into a SEARCH-RESPONSE."
  (when (aget json "error")
    (error 'search-error
           :format-control "Tavily API error: ~A"
           :format-arguments (list (aget json "error"))))
  (make-search-response
   :provider :tavily
   :query query
   :results (loop for item in (aget json "results")
                  collect (make-search-result
                           :title (aget item "title")
                           :url (aget item "url")
                           :content (aget item "content")
                           :score (aget item "score")))
   :raw json))

(defun tavily-search (provider query &key api-key (max-results 5) model &allow-other-keys)
  (declare (ignore model))
  (let ((headers '(("Content-Type" . "application/json")))
        (payload `(("api_key" . ,api-key)
                   ("query" . ,query)
                   ("max_results" . ,max-results))))
    (parse-tavily-response
     (%post-json (search-provider-base-url provider) headers payload)
     query)))

;;; ---- Perplexity: POST chat completion, answer plus citations ----

(defun parse-perplexity-response (json query)
  "Turn a decoded Perplexity chat response into a SEARCH-RESPONSE. The LLM
answer goes in the ANSWER slot; citations/search_results become RESULTS."
  (let* ((choice (first (aget json "choices")))
         (message (and choice (aget choice "message")))
         (search-results (aget json "search_results"))
         (citations (aget json "citations")))
    (make-search-response
     :provider :perplexity
     :query query
     :answer (and message (aget message "content"))
     :results (cond
                (search-results
                 (loop for item in search-results
                       collect (make-search-result
                                :title (aget item "title")
                                :url (aget item "url")
                                :content (aget item "snippet")
                                :score (aget item "score"))))
                (citations
                 (loop for url in citations
                       collect (make-search-result :url url)))
                (t nil))
     :raw json)))

(defun perplexity-search (provider query &key api-key model max-results &allow-other-keys)
  (declare (ignore max-results))
  (let ((headers (list (cons "Content-Type" "application/json")
                       (cons "Authorization" (concatenate 'string "Bearer " api-key))))
        (payload `(("model" . ,(or model "sonar-pro"))
                   ("messages" . ((("role" . "user")
                                   ("content" . ,query)))))))
    (parse-perplexity-response
     (%post-json (search-provider-base-url provider) headers payload)
     query)))

;;; ---- registration ----

(define-search-provider :brave "https://api.search.brave.com/res/v1/web/search"
  :env-keys '("BRAVE_SEARCH_API_KEY")
  :function #'brave-search)

(define-search-provider :tavily "https://api.tavily.com/search"
  :env-keys '("TAVILY_API_KEY")
  :function #'tavily-search)

(define-search-provider :perplexity "https://api.perplexity.ai/chat/completions"
  :env-keys '("PERPLEXITY_API_KEY")
  :function #'perplexity-search)
