;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:ollama
  (:use #:cl #:llm)
  (:export #:ollama-llm
           #:completions
           #:summarize
           #:answer-question))

(in-package #:ollama)

(defvar *ollama-endpoint* "http://localhost:11434/api/chat")
;;(defvar *ollama-model* "mistral:v0.3")
;;(defvar *ollama-model* "qwen3.5:9b")
(defvar *ollama-model* "qwen3.5:0.8b")

(defun completions (starter-text &key tools (model-id *ollama-model*) (think t))
  (let* ((tools-rendered
          (when tools
            (loop for tool-symbol in tools
                  collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                            (if tool
                                (simple-tools:render-tool tool)
                                (error "Undefined tool function: ~A" tool-symbol))))))
         (message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (data (list (cons :|model| model-id)
                     (cons :|stream| nil)
                     (cons :|think| think)
                     (cons :|messages| (list message))))
         (data-with-tools (if tools-rendered
                              (append data (list (cons :|tools| tools-rendered)))
                              data))
         (json-data (cl-json:encode-json-to-string data-with-tools))
         (fixed-json-data
          (llm:substitute-subseq json-data ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~a -d \"~A\""
                               *ollama-endpoint*
                               escaped-json)))
    (let ((response (llm:run-curl-command curl-command)))
      (with-input-from-string (s response)
        (let* ((json-as-list (cl-json:decode-json s))
               (message-resp (cdr (assoc :message json-as-list)))
               (tool-calls (cdr (assoc :tool--calls message-resp)))
               (content (cdr (assoc :content message-resp))))
          (if tool-calls
              (let ((results
                     (loop for call in tool-calls
                           collect (let* ((func (cdr (assoc :function call)))
                                         (name (cdr (assoc :name func)))
                                         (args (cdr (assoc :arguments func)))
                                         (tool (gethash name simple-tools:*tools*))
                                         (mapped-args (simple-tools:map-args-to-parameters tool args)))
                                     (apply (simple-tools:tool-fn tool) mapped-args)))))
                (format nil "~{~A~^~%~}" results))
              (or content "No response content")))))))

(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "Q: " some-text " A:")))
