;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:fireworks-ai
  (:use #:cl)
  (:export #:fireworks-llm
           #:completions
           #:answer-question))

(in-package #:fireworks-ai)

(defvar *fireworks-endpoint* "https://api.fireworks.ai/inference/v1/completions")
(defvar *fireworks-model* "accounts/fireworks/models/deepseek-v4-flash")

(defun get-fireworks-api-key ()
  (uiop:getenv "FIREWORKS_API_KEY"))

(defun completions (prompt &key (model-id *fireworks-model*) (max-tokens 131072)
                              (temperature 0.1) (top-p 1) (top-k 40)
                              (presence-penalty 0) (frequency-penalty 0))
  (let* ((data `((:model . ,model-id)
                 (:max--tokens . ,max-tokens)
                 (:top--p . ,top-p)
                 (:top--k . ,top-k)
                 (:presence--penalty . ,presence-penalty)
                 (:frequency--penalty . ,frequency-penalty)
                 (:temperature . ,temperature)
                 (:prompt . ,prompt)))
         (request-body (cl-json:encode-json-to-string data))
         (headers (list '("Content-Type" . "application/json")
                        (cons "Authorization" (concatenate 'string "Bearer " (get-fireworks-api-key)))))
         (response (dex:post *fireworks-endpoint* :headers headers :content request-body)))
    (with-input-from-string (s response)
      (let* ((json-as-list (cl-json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (text (cdr (assoc :text first-choice))))
        (or text "No response content")))))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))
