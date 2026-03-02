;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:llm
  (:use #:cl)
  (:export #:run-curl-command
           #:escape-json
           #:substitute-subseq))

(in-package #:llm)

(defun run-curl-command (curl-command)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program curl-command
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        output
        (error "Curl command failed: ~A~%Error: ~A" curl-command error-output))))

(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
          (if (char= ch #\")
              (write-string "\\\"" out)
              (if (char= ch #\\)
                  (write-string "\\\\" out)
                  (write-char ch out))))))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))
