(in-package #:ollama)

;;; Ollama Cloud agent with web_search and web_fetch tool calling using litelm.
;;; Requires OLLAMA_API_KEY to be set in the environment.

(defvar *cloud-model-name* "gpt-oss:120b-cloud")
(defvar *cloud-host* "https://ollama.com/v1")

;; Register Ollama Cloud provider with litelm
(eval-when (:load-toplevel :execute)
  (litelm:define-provider :ollama-cloud "https://ollama.com/v1"
    :env-keys '("OLLAMA_API_KEY")))

;;; Tool definitions in litelm format

(defvar *cloud-search-tools*
  '((web_search "Search the web for current information"
      ((query "string" "The search query string")))
    (web_fetch "Fetch the content of a web page by URL"
      ((url "string" "The URL to fetch")))))

;;; API key helper

(defun get-api-key ()
  "Read OLLAMA_API_KEY from the environment. Signals an error if not set."
  (or (uiop:getenv "OLLAMA_API_KEY")
      (error "OLLAMA_API_KEY environment variable is not set")))

;;; Tool execution
;;;
;;; The query and URL arguments come from the model, so curl is run with an
;;; argument list (no shell) and the query is URL-encoded; otherwise text like
;;; "$(...)" in a tool argument would be executed by the shell.

(defun run-curl (args)
  "Run curl with the list of string ARGS and return its output as a string."
  (uiop:run-program (cons "curl" args)
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun execute-web-search (args)
  "Search the web via DuckDuckGo. ARGS is an alist with :query key."
  (let* ((query (or (cdr (assoc :query args :test #'string-equal))
                    (cdr (assoc "query" args :test #'string-equal))
                    ""))
         (url (format nil
                      "https://api.duckduckgo.com/?q=~a&format=json&no_html=1&skip_disambig=1"
                      (quri:url-encode query))))
    (format t "  [web_search] query: ~a~%" query)
    (handler-case
        (let ((result (run-curl (list "-s" "--max-time" "10" url))))
          (format t "  [web_search] got ~a chars~%" (length result))
          result)
      (error (e) (format nil "web_search error: ~a" e)))))

(defun execute-web-fetch (args)
  "Fetch the content of a URL. ARGS is an alist with :url key."
  (let* ((url (or (cdr (assoc :url args :test #'string-equal))
                  (cdr (assoc "url" args :test #'string-equal))
                  "")))
    (format t "  [web_fetch] url: ~a~%" url)
    (handler-case
        (let ((result (if (or (uiop:string-prefix-p "http://" url)
                              (uiop:string-prefix-p "https://" url))
                          (run-curl (list "-s" "-L" "--max-time" "15" "--" url))
                          (error "only http:// and https:// URLs can be fetched"))))
          (format t "  [web_fetch] got ~a chars~%" (length result))
          ;; Limit size to avoid overwhelming the model context
          (subseq result 0 (min 4000 (length result))))
      (error (e) (format nil "web_fetch error: ~a" e)))))

;;; Agent loop using litelm

(defun cloud-search-agent (prompt &key (model *cloud-model-name*))
  "Agent loop: calls Ollama Cloud with web_search and web_fetch tools,
   executing any tool calls and feeding results back until the model
   returns a final answer. Returns the final answer string."
  (let ((messages (list (list :user prompt)))
        (full-model (if (find #\/ model)
                        model
                        (concatenate 'string "ollama-cloud/" model))))
    (loop
      (format t "~%Calling Ollama Cloud (~a)...~%" model)
      (let* ((resp (litelm:completion full-model
                                      :messages messages
                                      :tools *cloud-search-tools*
                                      :api-base *cloud-host*
                                      :api-key (get-api-key)))
             (content (litelm:response-content resp))
             (tool-calls (litelm:response-tool-calls resp)))
        (format t "Raw response: ~s~%" (litelm:response-raw resp))
        (cond
          ;; Model requested one or more tool calls
          (tool-calls
           (format t "~%Model requested ~a tool call(s).~%" (length tool-calls))
           ;; Append assistant message with tool calls to history
           (setf messages
                 (append messages
                         (list (list :assistant content :tool-calls tool-calls))))
           (dolist (tc tool-calls)
             (let* ((name (getf tc :name))
                    (args (getf tc :arguments))
                    (result
                      (cond
                        ((string-equal name "web_search") (execute-web-search args))
                        ((string-equal name "web_fetch")  (execute-web-fetch args))
                        (t (format nil "Unknown tool: ~a" name)))))
               (format t "  Tool ~a completed.~%" name)
               ;; Append tool result to history in litelm message format
               (setf messages
                     (append messages
                             (list (list :tool (format nil "~a" result)
                                         :tool-call-id (getf tc :id))))))))
          ;; No tool calls - this is the final answer
          (t
           (format t "~%Final Answer: ~a~%" content)
           (return (or content "No response"))))))))

