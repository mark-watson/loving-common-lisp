;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:openai
  (:use #:cl #:llm)
  (:export #:openai-llm
           #:completions
           #:answer-question))

(in-package #:openai)

(defvar *openai-endpoint* "https://api.openai.com/v1/chat/completions")
(defvar *openai-model* "gpt-4o-mini")

(defun get-openai-api-key ()
  (uiop:getenv "OPENAI_KEY"))

(defun completions (starter-text &optional tools (model-id *openai-model*))
  (let* ((tools-rendered (when tools
                           (loop for tool-symbol in tools
                                 collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                                           (if tool
                                               (simple-tools:render-tool tool)
                                               (error "Undefined tool function: ~A" tool-symbol))))))
         (messages (cond
                     ((stringp starter-text)
                      (list (list (cons :role "user")
                                  (cons :content starter-text))))
                     ((and (listp starter-text) (keywordp (caar starter-text)))
                      (list starter-text))
                     (t starter-text)))
         (base-data `((model . ,model-id)
                      (messages . ,messages)))
         (data (if tools-rendered
                   (append base-data (list (cons :tools tools-rendered)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (llm:substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
                               *openai-endpoint*
                               (get-openai-api-key)
                               escaped-json)))
    (let ((response (llm:run-curl-command curl-command)))
      (with-input-from-string (s response)
        (let* ((json-as-list (cl-json:decode-json s))
               (choices (cdr (assoc :choices json-as-list)))
               (first-choice (car choices))
               (message-resp (cdr (assoc :message first-choice)))
               (tool-calls (cdr (assoc :tool--calls message-resp)))
               (content (cdr (assoc :content message-resp))))
          (if tool-calls
              (let ((results
                     (loop for call in tool-calls
                           collect (let* ((func (cdr (assoc :function call)))
                                         (name (cdr (assoc :name func)))
                                         (args-json (cdr (assoc :arguments func)))
                                         (args (cl-json:decode-json-from-string args-json))
                                         (tool (gethash name simple-tools:*tools*))
                                         (mapped-args (simple-tools:map-args-to-parameters tool args)))
                                     (apply (simple-tools:tool-fn tool) mapped-args)))))
                (format nil "~{~A~^~%~}" results))
              (or content "No response content")))))))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))
