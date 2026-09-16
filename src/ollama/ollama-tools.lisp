(in-package #:ollama)

;;; Ollama completions with tool/function calling support
;;; Uses litelm for model routing, message handling, and tool schemas.

(defvar *tool-model-name* "qwen3-vl:2b")

(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters
  handler)  ;; Common Lisp function to handle the call

(defun register-tool-function (name description parameters handler)
  "Register a function that can be called by the LLM via tool calling.
   NAME is a string or symbol.
   PARAMETERS is a list of parameters in litelm format:
     ((param-name param-type param-description &key required enum) ...)
   HANDLER is a Common Lisp function that takes an alist of arguments."
  (let ((tool-name (string-downcase (string name))))
    (setf (gethash tool-name *available-functions*)
          (make-ollama-function
           :name tool-name
           :description description
           :parameters parameters
           :handler handler))))

(defun infer-function-name-from-args (args)
  "Infer the function name based on argument keys
   (workaround for models that return empty name)."
  (let ((arg-keys (mapcar (lambda (pair)
                            (string-downcase (string (car pair))))
                          args)))
    (cond
      ((member "location" arg-keys :test #'string=) "get_weather")
      ((member "expression" arg-keys :test #'string=) "calculate")
      (t nil))))

(defun handle-tool-function-call (function-call)
  "Handle a function call returned from the LLM
   by invoking the registered handler."
  (format t "~%DEBUG handle-tool-function-call: ~a~%" function-call)
  (let* ((raw-name (or (getf function-call :name)
                       (cdr (assoc :name function-call))))
         (args (or (getf function-call :arguments)
                   (cdr (assoc :arguments function-call))))
         ;; If name is empty, try to infer from arguments
         (name (if (or (null raw-name) (string= raw-name ""))
                   (infer-function-name-from-args args)
                   raw-name))
         (func (gethash (string-downcase (string name)) *available-functions*)))
    (format t "DEBUG raw-name=~a inferred-name=~a args=~a func=~a~%"
            raw-name name args func)
    (if func
        (let ((handler (ollama-function-handler func)))
          (if handler
              (funcall handler args)
              (format nil
                      "No handler for function ~a, args: ~a" name args)))
        (error "Unknown function: ~a" name))))

(defun %convert-to-litelm-tools (functions)
  "Convert registered tool names into litelm tool definitions:
   ((name description ((param-name param-type param-desc ...) ...)) ...)"
  (mapcar (lambda (f)
            (let* ((name-str (string-downcase (string f)))
                   (func (gethash name-str *available-functions*)))
              (unless func
                (error "Unknown tool function: ~a" f))
              (list (ollama-function-name func)
                    (ollama-function-description func)
                    (ollama-function-parameters func))))
          functions))

(defun completions-with-tools (starter-text &optional functions)
  "Completion with function/tool calling support.
   STARTER-TEXT is the prompt to send to the LLM.
   FUNCTIONS is an optional list of registered function names to make available."
  (let* ((full-model (ensure-model-name *tool-model-name*))
         (tool-defs (when functions
                      (%convert-to-litelm-tools functions)))
         (resp (litelm:completion full-model
                                  :messages starter-text
                                  :tools tool-defs
                                  :api-base *model-host*)))
    (format t "Raw response: ~s~%" (litelm:response-raw resp))
    (let ((tool-calls (litelm:response-tool-calls resp)))
      (if tool-calls
          (handle-tool-function-call (first tool-calls))
          (or (litelm:response-content resp) "No response content")))))

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (or (cdr (assoc :location args :test #'string-equal))
                      (cdr (assoc "location" args :test #'string-equal)))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (format t "calculate called with args: ~a~%" args)
  (let ((expression (or (cdr (assoc :expression args :test #'string-equal))
                        (cdr (assoc "expression" args :test #'string-equal)))))
    (if expression
        (handler-case
            (format nil "Result: ~a"
                    (eval (read-from-string expression)))
          (error (e) (format nil "Error calculating: ~a" e)))
        "No expression provided")))

;; Register sample functions with handlers
(register-tool-function
 "get_weather"
 "Get current weather for a location"
 '((location "string" "The city name"))
 #'get_weather)

(register-tool-function
 "calculate"
 "Perform a mathematical calculation"
 '((expression "string" "Math expression like 2 + 2"))
 #'calculate)