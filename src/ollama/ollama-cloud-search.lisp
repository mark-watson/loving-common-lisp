(in-package #:ollama)

;;; Ollama Cloud agent with web_search and web_fetch tool calling.
;;; Mirrors the Python example using the Ollama Cloud API.
;;; Requires OLLAMA_API_KEY to be set in the environment.

(defvar *cloud-model-name* "gpt-oss:120b-cloud")
(defvar *cloud-host* "https://ollama.com/api/chat")

;;; Tool schemas sent to the model

(defvar *web-search-tool-schema*
  (list (cons :|type| "function")
        (cons :|function|
              (list (cons :|name| "web_search")
                    (cons :|description| "Search the web for current information")
                    (cons :|parameters|
                          (list (cons :|type| "object")
                                (cons :|properties|
                                      (list (cons :|query|
                                                  (list (cons :|type| "string")
                                                        (cons :|description|
                                                              "The search query string")))))
                                (cons :|required| '("query"))))))))

(defvar *web-fetch-tool-schema*
  (list (cons :|type| "function")
        (cons :|function|
              (list (cons :|name| "web_fetch")
                    (cons :|description| "Fetch the content of a web page by URL")
                    (cons :|parameters|
                          (list (cons :|type| "object")
                                (cons :|properties|
                                      (list (cons :|url|
                                                  (list (cons :|type| "string")
                                                        (cons :|description|
                                                              "The URL to fetch")))))
                                (cons :|required| '("url"))))))))

;;; API key helper

(defun get-api-key ()
  "Read OLLAMA_API_KEY from the environment. Signals an error if not set."
  (or (uiop:getenv "OLLAMA_API_KEY")
      (error "OLLAMA_API_KEY environment variable is not set")))

;;; Tool execution

(defun execute-web-search (args)
  "Search the web via DuckDuckGo. ARGS is an alist with :query key."
  (let* ((query (or (cdr (assoc :query args)) ""))
         (encoded (substitute #\+ #\Space query))
         (url (format nil
                      "https://api.duckduckgo.com/?q=~a&format=json&no_html=1&skip_disambig=1"
                      encoded))
         (curl-cmd (format nil "curl -s --max-time 10 ~s" url)))
    (format t "  [web_search] query: ~a~%" query)
    (handler-case
        (let ((result (uiop:run-program curl-cmd :output :string :error-output :string)))
          (format t "  [web_search] got ~a chars~%" (length result))
          result)
      (error (e) (format nil "web_search error: ~a" e)))))

(defun execute-web-fetch (args)
  "Fetch the content of a URL. ARGS is an alist with :url key."
  (let* ((url (or (cdr (assoc :url args)) ""))
         (curl-cmd (format nil "curl -s -L --max-time 15 ~s" url)))
    (format t "  [web_fetch] url: ~a~%" url)
    (handler-case
        (let ((result (uiop:run-program curl-cmd :output :string :error-output :string)))
          (format t "  [web_fetch] got ~a chars~%" (length result))
          ;; Limit size to avoid overwhelming the model context
          (subseq result 0 (min 4000 (length result))))
      (error (e) (format nil "web_fetch error: ~a" e)))))

;;; Single API call to Ollama Cloud

(defun cloud-ollama-call (messages)
  "Make one chat request to Ollama Cloud with web_search/web_fetch tools.
   Returns (values content tool-calls raw-message-alist)."
  (let* ((api-key (get-api-key))
         (tools (list *web-search-tool-schema* *web-fetch-tool-schema*))
         (data (list (cons :|model| *cloud-model-name*)
                     (cons :|stream| nil)
                     (cons :|messages| messages)
                     (cons :|tools| tools)))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but stream needs false
         (fixed-json (substitute-subseq json-data ":null" ":false" :test #'string=))
         (auth-header (format nil "Authorization: Bearer ~a" api-key))
         (curl-cmd
           (format nil "curl -s -H ~s -H \"Content-Type: application/json\" ~a -d ~s"
                   auth-header
                   *cloud-host*
                   fixed-json)))
    (format t "~%Calling Ollama Cloud (~a)...~%" *cloud-model-name*)
    (handler-case
        (let ((response (uiop:run-program curl-cmd :output :string :error-output :string)))
          (format t "Raw response: ~a~%" response)
          (with-input-from-string (s response)
            (let* ((parsed (json:decode-json s))
                   ;; raw-message is an alist; re-encoding it preserves tool_calls
                   ;; because cl-json round-trips :TOOL--CALLS <-> "tool_calls"
                   (raw-message (cdr (assoc :message parsed)))
                   (content (cdr (assoc :content raw-message)))
                   (tool-calls (cdr (assoc :tool--calls raw-message))))
              (values content tool-calls raw-message))))
      (error (e)
        (format t "Error calling Ollama Cloud: ~a~%" e)
        (values nil nil nil)))))

;;; Agent loop

(defun cloud-search-agent (prompt)
  "Agent loop: calls Ollama Cloud with web_search and web_fetch tools,
   executing any tool calls and feeding results back until the model
   returns a final answer. Returns the final answer string."
  (let ((messages (list (list (cons :|role| "user")
                              (cons :|content| prompt)))))
    (loop
      (multiple-value-bind (content tool-calls raw-message)
          (cloud-ollama-call messages)
        ;; Append the model's response (including any tool_calls) to history.
        ;; raw-message is the cl-json decoded alist; re-encoding it is safe because
        ;; cl-json round-trips :ROLE -> "role", :TOOL--CALLS -> "tool_calls", etc.
        (when raw-message
          (setf messages (append messages (list raw-message))))

        (cond
          ;; Model requested one or more tool calls
          (tool-calls
           (format t "~%Model requested ~a tool call(s).~%" (length tool-calls))
           (dolist (tc tool-calls)
             (let* ((func (cdr (assoc :function tc)))
                    (name (cdr (assoc :name func)))
                    (args (cdr (assoc :arguments func)))
                    (result
                      (cond
                        ((string= name "web_search") (execute-web-search args))
                        ((string= name "web_fetch")  (execute-web-fetch args))
                        (t (format nil "Unknown tool: ~a" name)))))
               (format t "  Tool ~a completed.~%" name)
               ;; Append tool result to history (role "tool" per Ollama Cloud spec)
               (setf messages
                     (append messages
                             (list (list (cons :|role| "tool")
                                         (cons :|content| (format nil "~a" result))
                                         (cons :|tool--name| name)))))))
           ;; Loop back so the model can process the tool results
           )

          ;; No tool calls - this is the final answer
          (t
           (format t "~%Final Answer: ~a~%" content)
           (return (or content "No response"))))))))

;; Usage:
;; (setf (uiop:getenv "OLLAMA_API_KEY") "your-key-here")  ; or export in shell
;; (ollama::cloud-search-agent
;;   "What is the current price of Bitcoin and who is the CEO of Nvidia?")
