;;;; agent-system.lisp
;;;; A Common Lisp agent system using Grok API with support for tool calling.
;;;; Optionally uses Perplexity Sonar API for web search tool.
;;;;
;;;; Dependencies (load via Quicklisp):
;;;;   (ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))
;;;;
;;;; Usage:
;;;;   Set *grok-api-key* to your xAI Grok API key.
;;;;   Optionally set *perplexity-api-key* for web search support.
;;;;   Define custom tools using def-tool.
;;;;   Run (run-agent "Your query here")
;;;;
;;;; Note: This assumes Grok API is compatible with OpenAI-style chat completions.

(in-package :cl-user)

(ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))

(use-package :alexandria)

;; Required libraries
(require 'asdf)
(require 'uiop)

;; Configure YASON to handle symbol keys & values
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-string)

(defvar *grok-api-key*
  (uiop:getenv "X_GROK_API_KEY")
  "Your xAI Grok API key. Obtain from https://x.ai/api")

(defvar *perplexity-api-key* (uiop:getenv "PERPLEXITY_API_KEY")
  "Optional Perplexity AI API key for web search. If nil, web_search tool will error.")

(defvar *grok-base-url* "https://api.x.ai/v1"
  "Base URL for Grok API.")

(defvar *perplexity-base-url* "https://api.perplexity.ai"
  "Base URL for Perplexity API.")

(defvar *tools* (make-hash-table :test 'equal)
  "Hash table of tools: name -> (description parameters lisp-function)")

;; Configure cl+ssl to use updated CA certificates (adjust path as needed)
;; Example for macOS with certifi: /path/to/certifi/cacert.pem
;; (cl+ssl:ssl-load-global-verify-locations "/path/to/cacert.pem")
(warn "Ensure your system's CA certificates are up-to-date for SSL verification. Contact xAI support if SSL issues persist.")

(defun hash (&rest pairs)
  "Helper to create hash-table from pairs. Converts symbol or keyword keys to lowercase strings so YASON sees only string keys."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          for key = (if (symbolp k)
                        (string-downcase (symbol-name k))
                        k)
          do (setf (gethash key ht) v))
    ht))

(defmacro def-tool (name description parameters lisp-function)
  "Define a custom tool."
  `(setf (gethash ,name *tools*)
         (list ,description ,parameters ,lisp-function)))

;; Example tools

;; Web search tool using Perplexity (optional)
(def-tool "web_search"
  "Search the web for up-to-date information when needed. Use this for current events or real-time data."
  (hash :type "object"
        :properties (hash "query" (hash :type "string"
                                        :description "The search query string."))
        :required (list "query"))
  (lambda (args)
    (if *perplexity-api-key*
        (let* ((query (gethash "query" args))
               (messages (list (hash "role" "system"
                                     "content" "You are a helpful search assistant. Provide a concise answer based on web search.")
                               (hash "role" "user"
                                     "content" query)))
               (body (hash "model" "sonar"
                           "messages" messages
                           "max_tokens" 1024
                           "temperature" 0.7))
               (json-body (with-output-to-string (s) (yason:encode body s)))
               (raw nil) (status nil))
          ;; Call Perplexity
          (multiple-value-setq (raw status)
            (drakma:http-request
             (concatenate 'string *perplexity-base-url* "/chat/completions")
             :method :post
             :additional-headers `(("Authorization" . ,(concatenate 'string "Bearer " *perplexity-api-key*))
                                   ("Content-Type" . "application/json"))
             :content json-body
             :verify nil))
          ;; Convert to string if octet‑vector
          (let* ((body-str (if (vectorp raw)
                               (babel:octets-to-string raw :encoding :utf-8)
                               raw)))
            ;; DEBUG
            (format t "~&[web_search] Perplexity status=~a~%" status)
            (format t "[web_search] First 120 chars: ~a~%" (subseq body-str 0 (min 120 (length body-str))))
            ;; Handle non‑200 errors
            (unless (= status 200)
              (return
               (format nil "Web search failed (HTTP ~a): ~a" status body-str)))
            ;; Parse JSON
            (let* ((parsed (ignore-errors (yason:parse body-str)))
                   (choices (and (hash-table-p parsed) (gethash "choices" parsed))))
              (cond
                ((and choices (plusp (length choices)))
                 (let* ((choice (first choices))
                        (msg (and (hash-table-p choice) (gethash "message" choice)))
                        (content (and (hash-table-p msg) (gethash "content" msg))))
                   (if (and content (stringp content))
                       content
                       body-str)))
                (t
                 ;; Fallback: just return the whole response string
                 body-str))))))
        (error "Perplexity API key not set for web_search.")))

;; Example custom tool: get current date
(def-tool "get_current_date"
  "Get the current date in YYYY-MM-DD format."
  (hash :type "object" :properties (hash) :required #())
  (lambda (args)
    (declare (ignore args))
    (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
      (declare (ignore s m h))
      (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))))

;; Function to get tools in API format
(defun get-tools ()
  "Return list of tool schemas for API."
  (loop for name being the hash-keys of *tools*
        collect (destructuring-bind (desc params fn)
                    (gethash name *tools*)
                  (declare (ignore fn))
                  (hash "type" "function"
                        "function" (hash "name" name
                                         "description" desc
                                         "parameters" params)))))

(defun call-grok-chat (messages &key (model "grok-4") tools)
  (let ((body (hash "model"     model
                    "messages"  messages
                    "stream"    yason:false)))
    (when tools (setf (gethash "tools" body) tools))
    (let* ((json-body (with-output-to-string (s) (yason:encode body s)))
           (status nil) (raw nil))
      (multiple-value-setq (raw status)
        (drakma:http-request
         (concatenate 'string *grok-base-url* "/chat/completions")
         :method :post
         :additional-headers
           `(("Authorization" . ,(concatenate 'string "Bearer " *grok-api-key*)))
         :content       json-body
         :content-type  "application/json"
         :verify nil))
      (unless (= status 200)
        (error "Grok API returned status ~a: ~a" status raw))
      (let* ((body-str (if (vectorp raw)
                           (babel:octets-to-string raw :encoding :utf-8)
                           raw))
             (parsed   (yason:parse body-str)))
        parsed))))

(defun execute-tool (tool-call)
  "Execute a tool call and return the result string (or hash) from the invoked tool."
  (let* ((function-info (gethash "function" tool-call))
         (name          (gethash "name" function-info))
         (args-raw      (gethash "arguments" function-info))

         ;; Force ARGS-JSON to a true simple-string
         (args-json
           (cond
             ;; Character vector → simple-string
             ((and (vectorp args-raw) (every #'characterp args-raw))
              (coerce args-raw 'simple-string))

             ;; Already a string → coerce to simple-string to drop any adjustable/ fill‑pointer baggage
             ((stringp args-raw)
              (coerce args-raw 'simple-string))

             ;; Octet vector → decode UTF‑8
             ((vectorp args-raw)
              (babel:octets-to-string args-raw :encoding :utf-8))

             (t
              (error "Unexpected arguments payload type: ~s" (type-of args-raw)))))

         (tool-info (gethash name *tools*)))
    ;; DEBUG PRINTS ----------------------------------------------------------
    (format t "~&[execute-tool] name=~a args-raw type=~a~%" name (type-of args-raw))
    (cond
      ((stringp args-raw)
       (format t "[execute-tool] first 32 chars: ~a~%"
               (subseq args-raw 0 (min 32 (length args-raw)))))
      ((and (vectorp args-raw) (not (stringp args-raw)))
       (format t "[execute-tool] first 16 bytes: ~{~d~^ ~}~%"
               (subseq args-raw 0 (min 16 (length args-raw))))))
    (format t "[execute-tool] args-json final type=~a first 32: ~a~%"
            (type-of args-json)
            (subseq args-json 0 (min 32 (length args-json))))
    ;; ----------------------------------------------------------------------
    (let* ((args         (yason:parse args-json)))
      (if tool-info
          (let ((fn (third tool-info)))
            (funcall fn args))
          (error "Unknown tool: ~s" name)))))

(defun run-agent (query &key (model "grok-4") (system-prompt "You are a helpful agent that can use tools to answer questions."))
  "Run the agent loop for a query."
  (let ((messages (if system-prompt
                      (list (hash "role" "system" "content" system-prompt)
                            (hash "role" "user" "content" query))
                      (list (hash "role" "user" "content" query))))
        (tools (get-tools)))
    (loop
      (let ((response (call-grok-chat messages :model model :tools tools)))
        (let* ((choice (first (gethash "choices" response)))
               (message (gethash "message" choice))
               (finish-reason (gethash "finish_reason" choice)))
          (push message messages)  ;; Add assistant message to history
          (cond
            ;; Tool invocation (either explicit finish_reason or implicit via presence of tool_calls)
            ((or (member finish-reason '("tool_calls" "tool_call") :test #'equal)
                 (gethash "tool_calls" message))
             (let ((tool-calls (gethash "tool_calls" message)))
               (dolist (tool-call tool-calls)
                 (let* ((result (execute-tool tool-call))
                        (tool-response (hash "role" "tool"
                                             "tool_call_id" (gethash "id" tool-call)
                                             "name" (gethash "name" (gethash "function" tool-call))
                                             "content" result)))
                   (push tool-response messages)))))

            ;; Conversation finished
            ((or (equal finish-reason "stop")
                 ;; finish_reason NIL/"" → stop only if no tool_calls present
                 (and (or (null finish-reason) (equal finish-reason ""))
                      (not (gethash "tool_calls" message))))
             (return (gethash "content" message)))

            (t
             (error "Unknown finish reason: ~s" finish-reason))))))))

(trace call-grok-chat)
(trace execute-tool)
(trace get-tools)

;; (run-agent "what is 1 + 12?")
