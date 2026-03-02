;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:claude
  (:use #:cl #:llm)
  (:export #:claude-llm
           #:completions
           #:completions-with-search
           #:completions-with-search-and-citations
           #:answer-question))

(in-package #:claude)

(defvar *claude-endpoint* "https://api.anthropic.com/v1/messages")
(defvar *claude-model* "claude-sonnet-4-6")
(defvar *claude-max-tokens* 1000)

(defun get-claude-api-key ()
  (uiop:getenv "CLAUDE_API"))

(defun render-tool-for-claude (tool)
  "Render a tool as a JSON schema alist in Claude's input_schema format."
  (let* ((params (simple-tools:tool-parameters tool))
         (properties (loop for p in params
                           collect (let ((desc (third p)))
                                     (if desc
                                         (list (first p)
                                               (cons :type (second p))
                                               (cons :description desc))
                                         (list (first p)
                                               (cons :type (second p)))))))
         (required (loop for p in params collect (first p)))
         (schema (append '((:type . "object"))
                         (when properties (list (cons :properties properties)))
                         (when required (list (cons :required required))))))
    `((:name . ,(simple-tools:tool-name tool))
      (:description . ,(simple-tools:tool-description tool))
      (:input--schema . ,schema))))

(defun completions (starter-text &key tools (model-id *claude-model*))
  (let* ((tools-rendered (when tools
                           (loop for tool-symbol in tools
                                 collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                                           (if tool
                                               (render-tool-for-claude tool)
                                               (error "Undefined tool function: ~A" tool-symbol))))))
         (messages (cond
                     ((stringp starter-text)
                      (list `((:role . "user")
                              (:content . (((:type . "text") (:text . ,starter-text)))))))
                     (t starter-text)))
         (base-data `((:model . ,model-id)
                      (:max--tokens . ,*claude-max-tokens*)
                      (:temperature . 0)
                      (:messages . ,messages)))
         (data (if tools-rendered
                   (append base-data (list (cons :tools tools-rendered)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (llm:substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~A -H \"x-api-key: ~A\" -H \"anthropic-version: 2023-06-01\" -H \"content-type: application/json\" -d \"~A\""
                               *claude-endpoint*
                               (get-claude-api-key)
                               escaped-json)))
    (format t "$$ data:~%~A~%" data)
    (let ((response (llm:run-curl-command curl-command)))
      (with-input-from-string (s response)
        (let* ((json-as-list (cl-json:decode-json s))
               (content (cdr (assoc :content json-as-list)))
               (stop-reason (cdr (assoc :stop--reason json-as-list)))
               (tool-use-blocks (when (string= stop-reason "tool_use")
                                  (remove-if-not (lambda (block)
                                                   (string= (cdr (assoc :type block)) "tool_use"))
                                                 content))))
          (if tool-use-blocks
              (let ((results
                     (loop for block in tool-use-blocks
                           collect (let* ((name (cdr (assoc :name block)))
                                         (input (cdr (assoc :input block)))
                                         (tool (gethash name simple-tools:*tools*))
                                         (mapped-args (simple-tools:map-args-to-parameters tool input)))
                                     (apply (simple-tools:tool-fn tool) mapped-args)))))
                (format nil "~{~A~^~%~}" results))
              (let ((first-block (car content)))
                (or (cdr (assoc :text first-block)) "No response content"))))))))

(defun completions-with-search (prompt &optional (model-id *claude-model*))
  "Call Claude with the built-in web search tool enabled. Returns the text response."
  (let* ((messages (list `((:role . "user")
                           (:content . (((:type . "text") (:text . ,prompt)))))))
         (search-tool `((:type . "web_search_20250305") (:name . "web_search")))
         (data `((:model . ,model-id)
                 (:max--tokens . ,*claude-max-tokens*)
                 (:temperature . 0)
                 (:messages . ,messages)
                 (:tools . (,search-tool))))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (llm:substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~A -H \"x-api-key: ~A\" -H \"anthropic-version: 2023-06-01\" -H \"anthropic-beta: web-search-2025-03-05\" -H \"content-type: application/json\" -d \"~A\""
                               *claude-endpoint*
                               (get-claude-api-key)
                               escaped-json))
         (response (llm:run-curl-command curl-command)))
    (with-input-from-string (s response)
      (let* ((json-as-list (cl-json:decode-json s))
             (content (cdr (assoc :content json-as-list)))
             (text-blocks (remove-if-not (lambda (block)
                                           (string= (cdr (assoc :type block)) "text"))
                                         content))
             (last-text-block (car (last text-blocks))))
        (or (cdr (assoc :text last-text-block)) "No response content")))))

(defun completions-with-search-and-citations (prompt &optional (model-id *claude-model*))
  "Call Claude with the built-in web search tool enabled.
Returns (values text citations) where citations is a list of (title . url) pairs."
  (let* ((messages (list `((:role . "user")
                           (:content . (((:type . "text") (:text . ,prompt)))))))
         (search-tool `((:type . "web_search_20250305") (:name . "web_search")))
         (data `((:model . ,model-id)
                 (:max--tokens . ,*claude-max-tokens*)
                 (:temperature . 0)
                 (:messages . ,messages)
                 (:tools . (,search-tool))))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (llm:substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~A -H \"x-api-key: ~A\" -H \"anthropic-version: 2023-06-01\" -H \"anthropic-beta: web-search-2025-03-05\" -H \"content-type: application/json\" -d \"~A\""
                               *claude-endpoint*
                               (get-claude-api-key)
                               escaped-json))
         (response (llm:run-curl-command curl-command)))
    (with-input-from-string (s response)
      (let* ((json-as-list (cl-json:decode-json s))
             (content (cdr (assoc :content json-as-list)))
             (text-blocks (remove-if-not (lambda (block)
                                           (string= (cdr (assoc :type block)) "text"))
                                         content))
             (last-text-block (car (last text-blocks)))
             (text (or (cdr (assoc :text last-text-block)) "No response content"))
             (result-blocks (remove-if-not (lambda (block)
                                             (string= (cdr (assoc :type block)) "web_search_tool_result"))
                                           content))
             (citations (loop for block in result-blocks
                              for block-content = (cdr (assoc :content block))
                              append (loop for result in block-content
                                           when (string= (cdr (assoc :type result)) "web_search_result")
                                           collect (cons (cdr (assoc :title result))
                                                         (cdr (assoc :url result)))))))
        (values text citations)))))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))
