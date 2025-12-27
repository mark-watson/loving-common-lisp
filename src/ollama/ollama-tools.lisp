(in-package #:ollama)

;;; Ollama completions with tool/function calling support
;;; Uses shared utilities from ollama-helper.lisp

(defvar *tool-model-name* "mistral:v0.3")

(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters)

(defun register-tool-function (name description parameters)
  "Register a function that can be called by the LLM via tool calling."
  (setf (gethash name *available-functions*)
        (make-ollama-function
         :name name
         :description description
         :parameters parameters)))

(defun handle-tool-function-call (function-call)
  "Handle a function call returned from the LLM."
  (let* ((name (cdr (assoc :name function-call)))
         (args (cdr (assoc :arguments function-call)))
         (func (gethash name *available-functions*)))
    (if func
        (format nil "Function ~a called with args: ~a" name args)
        (error "Unknown function: ~a" name))))

(defun completions-with-tools (starter-text &optional functions)
  "Completion with function/tool calling support.
   STARTER-TEXT is the prompt to send to the LLM.
   FUNCTIONS is an optional list of registered function names to make available."
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :|name| (ollama-function-name func))
                                            (cons :|description| (ollama-function-description func))
                                            (cons :|parameters| (ollama-function-parameters func)))))
                                  functions)))
         (message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (base-data (list (cons :|model| *tool-model-name*)
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
                  ollama::*model-host*
                  fixed-json-data)))
    (multiple-value-bind (content function-call)
        (ollama-helper curl-command)
      (if function-call
          (handle-tool-function-call (car function-call))
          (or content "No response content")))))

;; Register sample functions
(register-tool-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|location| (list (cons :|type| "string")
                                                         (cons :|description| "The city name")))))
       (cons :|required| '("location"))))

(register-tool-function
 "calculate"
 "Perform a mathematical calculation"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|expression| (list (cons :|type| "string")
                                                           (cons :|description| "Math expression like 2 + 2")))))
       (cons :|required| '("expression"))))

;; Example call:
;;(ollama::completions-with-tools "Use function calling for: What's the weather like in New York?" '("get_weather" "calculate"))
;;;; function calling currently returns:
#|
Raw response: {"model":"qwen2.5:14b","created_at":"2025-03-07T18:24:47.736124Z","message":{"role":"assistant","content":"","tool_calls":[{"function":{"name":"get_weather","arguments":{"location":"New York, US"}}}]},"done_reason":"stop","done":true,"total_duration":14937296042,"load_duration":12617250,"prompt_eval_count":174,"prompt_eval_duration":447000000,"eval_count":246,"eval_duration":14476000000}
|#
