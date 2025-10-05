# More Agents Using X’s Grok and Perplexity APIs

TBD

## Agent Using X’s Grok API

TBD

File **agent.lisp**:

```lisp
;;;; agent-system.lisp
;;;; A Common Lisp agent system using Grok API with support for tool calling.
;;;;
;;;; Dependencies (load via Quicklisp):
;;;;   (ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))
;;;;
;;;; Usage:
;;;;   Set *grok-api-key* to your xAI Grok API key.
;;;;   Define custom tools using def-tool.
;;;;   Run (run-agent "Your query here")
;;;;
;;;; Note: This assumes Grok API is compatible with OpenAI-style chat completions.

(in-package :cl-user)

(ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))

;; Required libraries
(require 'asdf)
(require 'uiop)

;; Configure YASON to handle symbol keys & values
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-string)

(defvar *grok-api-key*
  (uiop:getenv "X_GROK_API_KEY")
  "Your xAI Grok API key. Obtain from https://x.ai/api")

(defvar *grok-base-url* "https://api.x.ai/v1"
  "Base URL for Grok API.")

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
;; (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play?")
```

TBD

Let’s run the two examples at the bottom of the last listing:

```lisp
CL-USER 1 > (load "agent.lisp")
CL-USER 2 > (run-agent "what is 1 + 12?")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 801002623B>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 8010017C03> #<EQUAL Hash Table{2} 801001A013>)
  >> MODEL    : "grok-4"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002623B>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 801009F0E3>
"13"
T

CL-USER 3 > (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web.")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 80100AF723>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 80100A23CB> #<EQUAL Hash Table{2} 80100A4763>)
  >> MODEL    : "grok-4"
  >> TOOLS    : (#<EQUAL Hash Table{2} 80100AF723>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100CFF4B>
"Yes, that's accurate! Mark Watson is a software consultant and author with a strong background in artificial intelligence, programming languages like Common Lisp, and technologies such as the Semantic Web. Some of his notable books include:

- **Loving Common Lisp, or the Savvy Programmer's Secret Weapon** (on Lisp programming).
- **Practical Semantic Web and Linked Data Applications** (focusing on Semantic Web technologies).
- **Practical Artificial Intelligence Programming With Java** (covering AI concepts).

He's written over 20 books in total, often emphasizing practical, hands-on approaches to these subjects. If you're interested in recommendations, specific book details, or more about his work, let me know!"
T

CL-USER 4 >
```

Here we are using the innate knowledge in X’1 Grok model.

## Agent Using X’s Grok API and Perplexity’s Search API

TBD

This agent example is a work in progress and currently running the agent results in hundreds of lines of debug printout.

File ** agent_grok_perplexity.lisp**:

```lisp
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

(defun pp-hash (ht &optional (stream *standard-output*) (indent 0))
  "Pretty-print hash table HT to STREAM, indenting by INDENT spaces."
  (let ((keys (loop for k being the hash-keys of ht collect k)))
    (format stream "~&~v@{~}" indent "")     ; indent
    (format stream "#HASH{~%")
    (let ((next-indent (+ indent 2)))
      (dolist (k keys)
        (let ((v (gethash k ht)))
          (format stream "~v@{~}" next-indent "")
          (format stream "~S => ~S~%" k v)))
      (format stream "~v@{~}" indent "")
      (format stream "}") )
  ht))

(defmacro def-tool (name description parameters lisp-function)
  "Define a custom tool."
  `(setf (gethash ,name *tools*)
         (list ,description ,parameters ,lisp-function)))

;; Example tools

(defvar *x* nil) ;; DEBUG

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
            (format t "[web_search] First 8192 chars: ~a~%" (subseq body-str 0 (min 8192 (length body-str))))
            ;; Handle non‑200 errors
            (unless (= status 200)
              (return
               (format nil "Web search failed (HTTP ~a): ~a" status body-str)))
            ;; Parse JSON
	    (setf *x* (yason:parse body-str)) ;; DEBUG ONLY
            (let* ((parsed (ignore-errors (yason:parse body-str)))
                   (choices (and (hash-table-p parsed) (gethash "choices" parsed))))
	      (format t "~%[web_search] choices=~%~A~%" choices)
              (cond
                ((and choices (plusp (length choices)))
                 (let* ((choice (first choices))
                        (msg (and (hash-table-p choice) (gethash "message" choice)))
                        (content (and (hash-table-p msg) (gethash "content" msg))))
		   (format t "~%[web_search] content=~%~A~%" content)
		   content)))))))))
        ;;            (if (and content (stringp content))
        ;;                content
        ;;                body-str)))
        ;;         (t
        ;;          ;; Fallback: just return the whole response string
        ;;          body-str))))))
        ;; (error "Perplexity API error.")))

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
;; (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play? Return only a list of musical instruments.")
```

TBD

Let’s run another example that requires a web search since Grok’s innate knowledge can’t answer the query:

```
CL-USER 1 > (load "agent_grok_perplexity.lisp")
CL-USER 3 > (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play? Return only a list of musical instruments.")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 8010015043> #<EQUAL Hash Table{2} 80100173DB>)
  >> MODEL    : "grok-4"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100A46EB>
0 EXECUTE-TOOL > ...
  >> TOOL-CALL : #<EQUAL Hash Table{3} 80100A625B>
[execute-tool] name=web_search args-raw type=(ARRAY CHARACTER (80))
[execute-tool] first 32 chars: {"query":"Mark Watson AI Lisp se
[execute-tool] args-json final type=SIMPLE-TEXT-STRING first 32: {"query":"Mark Watson AI Lisp se
[web_search] Perplexity status=200
[web_search] First 8192 chars: {"id": "ecc7aa86-4434-4d70-8dfa-e85b7c7e27d6", "model": "sonar", "created": 1759702702, "usage": {"prompt_tokens": 27, "completion_tokens": 230, "total_tokens": 257, "search_context_size": "low", "cost": {"input_tokens_cost": 0.0, "output_tokens_cost": 0.0, "request_cost": 0.005, "total_cost": 0.005}}, "citations": ["https://leanpub.com/hy-lisp-python", "https://markwatson.com/llms.txt", "https://creativecommons.org/2005/07/01/watson/", "https://www.goodreads.com/author/list/5182666.Mark_Watson", "https://github.com/mark-watson/lisp_practical_semantic_web", "https://www.chessprogramming.org/Mark_Watson", "https://markwatson.com", "https://mark-watson.blogspot.com", "https://leanpub.com/u/markwatson", "https://markwatson.com/opencontent/book_lisp.pdf", "https://github.com/mark-watson/free-older-books-and-software"], "search_results": [{"title": "A Lisp Programmer Living in\u2026 by Mark Watson [PDF/iPad/Kindle]", "url": "https://leanpub.com/hy-lisp-python", "date": "2025-08-20", "last_updated": "2025-09-27", "snippet": "A Lisp Programmer Living in Python-Land: The Hy Programming Language. Use Hy with Large Language Models, Semantic Web, Web Scraping, Web Search, ...", "source": "web"}, {"title": "https://markwatson.com/llms.txt", "url": "https://markwatson.com/llms.txt", "date": null, "last_updated": "2025-10-05", "snippet": "Mark Watson's hobbies are cooking, photography, hiking, travel, and playing the following musical instruments: guitar, didgeridoo, and American Indian flute.", "source": "web"}, {"title": "Mark Watson - Creative Commons", "url": "https://creativecommons.org/2005/07/01/watson/", "date": "2005-07-01", "last_updated": "2024-09-26", "snippet": "Mark Watson is an accomplished programmer and writer of thirteen books on various technical topics. An expert in artificial intelligence and language ...", "source": "web"}, {"title": "Books by Mark Watson (Author of Loving Common Lisp ... - Goodreads", "url": "https://www.goodreads.com/author/list/5182666.Mark_Watson", "date": "2025-10-01", "last_updated": "2025-10-05", "snippet": "Mark Watson has 31 books on Goodreads with 379 ratings. Mark Watson's most popular book is Loving Common Lisp, or the Savvy Programmer's Secret Weapon.", "source": "web"}, {"title": "Examples from the Lisp version of my semantic web book - GitHub", "url": "https://github.com/mark-watson/lisp_practical_semantic_web", "date": "2011-10-23", "last_updated": "2025-06-13", "snippet": "Examples from the Lisp version of my semantic web book. markwatson.com \u00b7 37 stars 8 forks Branches Tags Activity.", "source": "web"}, {"title": "Mark Watson - Chessprogramming wiki", "url": "https://www.chessprogramming.org/Mark_Watson", "date": "2003-03-27", "last_updated": "2025-02-12", "snippet": "Mark Watson, an American computer scientist, programmer, consultant and author of books on Artificial Intelligence, Java, Ruby, Common LISP, Semantic Web, NLP, ...", "source": "web"}, {"title": "Mark Watson: AI Practitioner and Author of 20+ AI Books | Mark ...", "url": "https://markwatson.com", "date": "2023-04-18", "last_updated": "2025-10-05", "snippet": "I am the author of 20+ books on Artificial Intelligence, Python, Common Lisp, Deep Learning, Haskell, Clojure, Java, Ruby, Hy language, and the Semantic Web ...", "source": "web"}, {"title": "Mark Watson's artificial intelligence and Lisp hacking blog", "url": "https://mark-watson.blogspot.com", "date": "2025-07-16", "last_updated": "2025-10-05", "snippet": "I am a consultant and the author of 20+ books on artificial intelligence, machine learning, and the semantic web. 55 US patents. My favorite languages are ...", "source": "web"}, {"title": "Mark Watson - Leanpub", "url": "https://leanpub.com/u/markwatson", "date": null, "last_updated": "2025-10-05", "snippet": "He is the author of 20+ published books on Artificial Intelligence, Deep Learning, Java, Ruby, Machine Learning, Common LISP, Clojure, JavaScript, Semantic Web, ...", "source": "web"}, {"title": "[PDF] Practical Semantic Web and Linked Data Applications - Mark Watson", "url": "https://markwatson.com/opencontent/book_lisp.pdf", "date": "2010-11-03", "last_updated": "2025-09-24", "snippet": "The broader purpose of this book is to provide application programming examples using AllegroGraph and. Linked Data sources on the web. This ...", "source": "web"}, {"title": "GitHub - mark-watson/free-older-books-and-software", "url": "https://github.com/mark-watson/free-older-books-and-software", "date": "2023-05-09", "last_updated": "2025-02-21", "snippet": "Mark Watson: AI Practitioner and Consultant Specializing in Large Language Models, LangChain/Llama-Index Integrations, Deep Learning, and the Semantic Web.", "source": "web"}], "object": "chat.completion", "choices": [{"index": 0, "finish_reason": "stop", "message": {"role": "assistant", "content": "Mark Watson is an AI practitioner, author, and programmer specializing in Lisp, semantic web, and large language models. He is the author of numerous books on AI, Lisp, semantic web technologies, and programming languages. He has worked extensively on semantic web and linked data applications, including a Common Lisp version of his semantic web book, and he integrates AI tools like OpenAI GPT and LangChain in his work[1][5][6].\n\nRegarding musical instruments, Mark Watson plays the **guitar, didgeridoo, and American Indian flute** as part of his hobbies[2].\n\nIn summary:\n\n| Aspect                     | Details                                               |\n|----------------------------|-------------------------------------------------------|\n| Profession                 | AI practitioner, Lisp programmer, semantic web author |\n| Key Contributions          | Books on AI, Lisp, semantic web; projects using Lisp and AI |\n| Semantic Web Work          | Practical Semantic Web and Linked Data Applications (Common Lisp and others) |\n| Musical Instruments Played | Guitar, didgeridoo, American Indian flute             |\n\nThis information is based on Mark Watson's personal website, books, and profiles[1][2][5][6]."}, "delta": {"role": "assistant", "content": ""}}]}

[web_search] choices=
(#<EQUAL Hash Table{4} 80100DD84B>)

[web_search] content=
Mark Watson is an AI practitioner, author, and programmer specializing in Lisp, semantic web, and large language models. He is the author of numerous books on AI, Lisp, semantic web technologies, and programming languages. He has worked extensively on semantic web and linked data applications, including a Common Lisp version of his semantic web book, and he integrates AI tools like OpenAI GPT and LangChain in his work[1][5][6].

Regarding musical instruments, Mark Watson plays the **guitar, didgeridoo, and American Indian flute** as part of his hobbies[2].

In summary:

| Aspect                     | Details                                               |
|----------------------------|-------------------------------------------------------|
| Profession                 | AI practitioner, Lisp programmer, semantic web author |
| Key Contributions          | Books on AI, Lisp, semantic web; projects using Lisp and AI |
| Semantic Web Work          | Practical Semantic Web and Linked Data Applications (Common Lisp and others) |
| Musical Instruments Played | Guitar, didgeridoo, American Indian flute             |

This information is based on Mark Watson's personal website, books, and profiles[1][2][5][6].
0 EXECUTE-TOOL < ...
  << VALUE-0 : "Mark Watson is an AI practitioner, author, and programmer specializing in Lisp, semantic web, and large language models. He is the author of numerous books on AI, Lisp, semantic web technologies, and programming languages. He has worked extensively on semantic web and linked data applications, including a Common Lisp version of his semantic web book, and he integrates AI tools like OpenAI GPT and LangChain in his work[1][5][6].

Regarding musical instruments, Mark Watson plays the **guitar, didgeridoo, and American Indian flute** as part of his hobbies[2].

In summary:

| Aspect                     | Details                                               |
|----------------------------|-------------------------------------------------------|
| Profession                 | AI practitioner, Lisp programmer, semantic web author |
| Key Contributions          | Books on AI, Lisp, semantic web; projects using Lisp and AI |
| Semantic Web Work          | Practical Semantic Web and Linked Data Applications (Common Lisp and others) |
| Musical Instruments Played | Guitar, didgeridoo, American Indian flute             |

This information is based on Mark Watson's personal website, books, and profiles[1][2][5][6]."
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{4} 80100DE1A3> #<EQUAL Hash Table{4} 80100A5F73> #<EQUAL Hash Table{2} 8010015043> #<EQUAL Hash Table{2} 80100173DB>)
  >> MODEL    : "grok-4"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100EA7C3>
"- Guitar
- Didgeridoo
- American Indian flute"
T

CL-USER 4 > 
```
