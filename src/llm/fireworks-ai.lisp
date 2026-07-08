;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:fireworks-ai
  (:use #:cl)
  (:export #:*fireworks-endpoint*
           #:*fireworks-model*
           #:fireworks-llm
           #:completions
           #:chat
           #:answer-question
           #:test-completions
           #:test-tools))

(in-package #:fireworks-ai)

(defvar *fireworks-endpoint*
  "https://api.fireworks.ai/inference/v1/chat/completions")
(defvar *fireworks-model*
  "accounts/fireworks/models/deepseek-v4-flash")

(defun get-fireworks-api-key ()
  (uiop:getenv "FIREWORKS_API_KEY"))

(defun make-headers ()
  "Build HTTP headers for Fireworks API requests."
  (list '("Accept" . "application/json")
        '("Content-Type" . "application/json")
        (cons "Authorization"
              (concatenate 'string "Bearer "
                (get-fireworks-api-key)))))

(defun %curl-post (url headers body)
  "Make a POST request using curl. HEADERS is an alist of (key . value) pairs."
  (let* ((header-flags
          (with-output-to-string (out)
            (loop for (key . value) in headers
                  do (format out " -H ~s"
                             (format nil "~a: ~a" key value)))))
         (cmd (format nil "curl -s~a -d ~s ~a" header-flags body url))
         (process (uiop:launch-program cmd
                                       :output :stream
                                       :error-output :stream))
         (response (with-output-to-string (out)
                     (loop for line
                             = (read-line (uiop:process-info-output process) nil nil)
                           while line
                           do (write-line line out)))))
    response))

(defun completions (prompt &key tools
                    (model-id *fireworks-model*)
                    (max-tokens 4096)
                    (temperature 0.6))
  "Send a chat completion to Fireworks AI.
PROMPT is a string. TOOLS is an optional list of
tool name strings registered in simple-tools:*tools*.
When the model invokes tools, a two-turn protocol
executes them locally and sends results back for
a final natural-language response."
  (let* ((tools-rendered
           (when tools
             (loop for tname in tools
                   collect
                   (let ((tool (gethash
                                 (string tname)
                                 simple-tools:*tools*)))
                     (if tool
                         (simple-tools:render-tool tool)
                         (error "Undefined tool: ~A"
                                tname))))))
         (messages
           (list `((:role . "user")
                   (:content . ,prompt))))
         (base-data
           `((:model . ,model-id)
             (:max--tokens . ,max-tokens)
             (:temperature . ,temperature)
             (:messages . ,messages)))
         (data (if tools-rendered
                   (append base-data
                     `((:tools . ,tools-rendered)
                       (:tool--choice . "auto")))
                   base-data))
         (request-body
           (cl-json:encode-json-to-string data))
         (headers (make-headers))
         (response
           (%curl-post *fireworks-endpoint* headers request-body)))
    (with-input-from-string (s response)
      (let* ((json (cl-json:decode-json s))
             (choices
               (cdr (assoc :choices json)))
             (first-choice (car choices))
             (message
               (cdr (assoc :message first-choice)))
             (tool-calls
               (cdr (assoc :tool--calls message)))
             (content
               (cdr (assoc :content message))))
        (if tool-calls
            (handle-tool-calls
              prompt message tool-calls
              tools-rendered headers
              model-id max-tokens temperature)
            (or content
                "No response content"))))))

(defun handle-tool-calls (original-prompt
                          model-message tool-calls
                          tools-rendered headers
                          model-id max-tokens
                          temperature)
  "Execute tool calls locally and send results back
to Fireworks for a final natural-language response.
Implements the two-turn tool-calling protocol."
  (let* ((tool-results
           (loop for call in tool-calls
                 collect
                 (let* ((call-id
                          (cdr (assoc :id call)))
                        (func
                          (cdr
                            (assoc :function call)))
                        (name
                          (cdr (assoc :name func)))
                        (args-json
                          (cdr
                            (assoc :arguments func)))
                        (args
                          (cl-json:decode-json-from-string
                            args-json))
                        (tool
                          (gethash name
                            simple-tools:*tools*))
                        (mapped
                          (simple-tools:map-args-to-parameters
                            tool args))
                        (result
                          (apply
                            (simple-tools:tool-fn tool)
                            mapped)))
                   (list call-id name
                         (princ-to-string result)))))
         ;; Build full conversation history:
         ;; user prompt, assistant tool invocation,
         ;; and tool result messages.
         (messages
           (append
             (list `((:role . "user")
                     (:content . ,original-prompt)))
             (list model-message)
             (loop for (id name result)
                     in tool-results
                   collect
                   `((:role . "tool")
                     (:tool--call--id . ,id)
                     (:name . ,name)
                     (:content . ,result)))))
         (data
           `((:model . ,model-id)
             (:max--tokens . ,max-tokens)
             (:temperature . ,temperature)
             (:messages . ,messages)
             (:tools . ,tools-rendered)
             (:tool--choice . "auto")))
         (request-body
           (cl-json:encode-json-to-string data))
         (response
           (%curl-post *fireworks-endpoint* headers request-body)))
    (with-input-from-string (s response)
      (let* ((json (cl-json:decode-json s))
             (choices
               (cdr (assoc :choices json)))
             (first-choice (car choices))
             (message
               (cdr (assoc :message first-choice)))
             (content
               (cdr (assoc :content message))))
        (or content "No response content")))))

(defun chat (messages &key (model-id *fireworks-model*)
                          (max-tokens 4096)
                          (temperature 0.6))
  "Send a multi-turn chat conversation to Fireworks AI.
MESSAGES is a list of alists, each with :role and :content keys,
e.g. ((:role . \"system\") (:content . \"...\"))"
  (let* ((data `((:model . ,model-id)
                 (:max--tokens . ,max-tokens)
                 (:temperature . ,temperature)
                 (:messages . ,messages)))
         (request-body (cl-json:encode-json-to-string data))
         (headers (make-headers))
         (response (%curl-post *fireworks-endpoint* headers request-body)))
    (with-input-from-string (s response)
      (let* ((json (cl-json:decode-json s))
             (choices (cdr (assoc :choices json)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (content (cdr (assoc :content message))))
        (or content "No response content")))))

(defun answer-question (question)
  (completions
    (concatenate 'string
      "Concisely answer the question: " question)))

;;; --- Test Functions ---

(defun test-completions ()
  "Test basic completion without tools."
  (format t "~%=== Fireworks: Basic Completion ===~%")
  (let ((result
          (completions
            "What is 2+2? Answer in one word.")))
    (format t "Response: ~A~%" result)
    result))

(defun test-tools ()
  "Test two-turn tool calling with a simulated
weather tool."
  (format t "~%=== Fireworks: Tool Calling ===~%")
  (simple-tools:define-tool "get_weather"
      (("location" "string" "City name"))
    "Get the current weather for a location"
    (format nil
      "{\"location\": \"~A\", ~
       \"temperature\": \"72F\", ~
       \"condition\": \"Sunny\"}"
      location))
  (let ((result
          (completions
            "What is the weather like in Tokyo?"
            :tools '("get_weather"))))
    (format t "Response: ~A~%" result)
    result))
