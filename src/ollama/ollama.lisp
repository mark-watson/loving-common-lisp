(in-package #:ollama)

(defvar *model-host* "http://localhost:11434/api/chat")
(defvar *model-name* "qwen2.5:14b") ;; "mistral:v0.3")
(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters)

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun register-function (name description parameters)
  (setf (gethash name *available-functions*)
        (make-ollama-function
         :name name
         :description description
         :parameters parameters)))

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
                 (function-call (cdr (assoc :tool_calls json-as-list)))
                 (content (cdr (assoc :content message))))
            (if function-call
                (handle-function-call (car function-call))
                (or content "No response content")))))
    (error (e)
      (format t "Error executing curl command: ~a~%" e)
      nil)))

(defun handle-function-call (function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args (cdr (assoc :arguments function-call)))
         (func (gethash name *available-functions*)))
    (if func
        (format nil "Function ~a called with args: ~a" name args)
        (error "Unknown function: ~a" name))))

(defun completions (starter-text &optional functions)
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :|name| (ollama-function-name func))
                                            (cons :|description| (ollama-function-description func))
                                            (cons :|parameters| (ollama-function-parameters func)))))
                                  functions)))
         (message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (base-data (list (cons :|model| *model-name*)
                          (cons :|stream| nil)
                          (cons :|messages| (list message))))
         (data (if function-defs
                   (append base-data (list (cons :|tools| function-defs)))
                   base-data))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but we need false for stream
         (fixed-json-data (substitute-subseq json-data ":null" ":false" :test #'string=))
         (curl-command
          (format nil "curl ~a -d ~s"
                  *model-host*
                  fixed-json-data)))
    (ollama-helper curl-command)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

;; Register sample functions
(register-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|location| (list (cons :|type| "string")
                                                         (cons :|description| "The city name")))))
       (cons :|required| '("location"))))

(register-function
 "calculate"
 "Perform a mathematical calculation"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|expression| (list (cons :|type| "string")
                                                           (cons :|description| "Math expression like 2 + 2")))))
       (cons :|required| '("expression"))))

;; Example call:
;;(ollama::completions "Use function calling for: What's the weather like in New York?" '("get_weather" "calculate"))
;;;; function calling currently returns:
#|
Raw response: {"model":"qwen2.5:14b","created_at":"2025-03-07T18:24:47.736124Z","message":{"role":"assistant","content":"","tool_calls":[{"function":{"name":"get_weather","arguments":{"location":"New York, US"}}}]},"done_reason":"stop","done":true,"total_duration":14937296042,"load_duration":12617250,"prompt_eval_count":174,"prompt_eval_duration":447000000,"eval_count":246,"eval_duration":14476000000}
|#

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "\nQ: " some-text "\nA:")))
