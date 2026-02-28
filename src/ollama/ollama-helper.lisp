(in-package #:ollama)

(defvar *model-host* "http://localhost:11434/api/chat")

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun ollama-helper (curl-command)
  (princ curl-command)
  (terpri)
  (handler-case
      (let ((response
             (uiop:run-program
              curl-command
              :output :string
              :error-output :string)))
        (princ "Raw response: ")
        (princ response)
        (terpri)
        (with-input-from-string
            (s response)
          (let* ((json-as-list (json:decode-json s))
                 (message (cdr (assoc :message json-as-list)))
                 (tool-calls (cdr (assoc :tool--calls message)))
                 (content (cdr (assoc :content message)))
                 ;; Extract function details from each tool_call
                 (function-calls (mapcar (lambda (tc)
                                           (cdr (assoc :function tc)))
                                         tool-calls)))
            (values content function-calls))))
    (error (e)
      (format t "Error executing curl command: ~a~%" e)
      nil)))
