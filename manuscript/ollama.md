# Local LLMs Using Ollama

*NOTE: as of December 27, 2025 this chapter is a work in progress.*

TBD

The **ollama** package developed here provides generative AI code and tool use/function calling generative AI code in the directory **loving-common-lisp/src/ollama**.

## Design Notes

TBD

## Implementation of Common Helper Code

TBD

Listing of package.lisp:

```lisp
;;;; package.lisp

(defpackage #:ollama
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:completions-with-tools #:summarize
           #:answer-question #:embeddings #:dot-product
           *model-name* *tool-model-name* *model-host*))
```

Listing of ollama.asd:

```lisp
;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "ollama-helper")
               (:file "ollama-tools") 
               (:file "ollama")))
```


Listing of ollama-helper.lisp:

```lisp
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
```


## Implementation of Generative AI Functionality

TBD

Listing of ollama.lisp:

```lisp
(in-package #:ollama)

;;; Basic Ollama completions without tool calling support
;;; For tool calling, see ollama-tools.lisp

(defvar *model-name* "mistral:v0.3")

(defun completions (starter-text)
  "Simple completion without function/tool calling support."
  (let* ((message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (data (list (cons :|model| *model-name*)
                     (cons :|stream| nil)
                     (cons :|messages| (list message))))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but we need false for stream
         (fixed-json-data (substitute-subseq json-data ":null" ":false" :test #'string=))
         (curl-command
          (format nil "curl ~a -d ~s"
                  ollama::*model-host*
                  fixed-json-data)))
    (multiple-value-bind (content function-call)
        (ollama-helper curl-command)
      (declare (ignore function-call))
      (or content "No response content"))))

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "
Q: " some-text "
A:")))
```

Sample output:

TBD



## Implementation of Tool Use/Function Calling Generative AI Functionality


TBD

Listing of ollama-tools.lisp:

```lisp
(in-package #:ollama)

;;; Ollama completions with tool/function calling support
;;; Uses shared utilities from ollama-helper.lisp

(defvar *tool-model-name* "mistral:v0.3")

(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters
  handler)  ;; Common Lisp function to handle the call

(defun register-tool-function (name description parameters handler)
  "Register a function that can be called by the LLM via tool calling.
   HANDLER is a Common Lisp function that takes a plist of arguments."
  (setf (gethash name *available-functions*)
        (make-ollama-function
         :name name
         :description description
         :parameters parameters
         :handler handler)))

(defun infer-function-name-from-args (args)
  "Infer the function name based on argument keys (workaround for models that return empty name)."
  (let ((arg-keys (mapcar #'car args)))
    (cond
      ((member :location arg-keys) "get_weather")
      ((member :expression arg-keys) "calculate")
      (t nil))))

(defun handle-tool-function-call (function-call)
  "Handle a function call returned from the LLM by invoking the registered handler."
  (format t "~%DEBUG handle-tool-function-call: ~a~%" function-call)
  (let* ((raw-name (cdr (assoc :name function-call)))
         (args (cdr (assoc :arguments function-call)))
         ;; If name is empty, try to infer from arguments
         (name (if (or (null raw-name) (string= raw-name ""))
                   (infer-function-name-from-args args)
                   raw-name))
         (func (gethash name *available-functions*)))
    (format t "DEBUG raw-name=~a inferred-name=~a args=~a func=~a~%" raw-name name args func)
    (if func
        (let ((handler (ollama-function-handler func)))
          (if handler
              (funcall handler args)
              (format nil "No handler for function ~a, args: ~a" name args)))
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

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (cdr (assoc :location args))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (let ((expression (cdr (assoc :expression args))))
    (if expression
        (handler-case
            (format nil "Result: ~a" (eval (read-from-string expression)))
          (error (e) (format nil "Error calculating: ~a" e)))
        "No expression provided")))

;; Register sample functions with handlers
(register-tool-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|location| (list (cons :|type| "string")
                                                         (cons :|description| "The city name")))))
       (cons :|required| '("location")))
 #'get_weather)

(register-tool-function
 "calculate"
 "Perform a mathematical calculation"
 (list (cons :|type| "object")
       (cons :|properties| (list (cons :|expression| (list (cons :|type| "string")
                                                           (cons :|description| "Math expression like 2 + 2")))))
       (cons :|required| '("expression")))
 #'calculate)

;; Example call:

#|
* (ollama::completions-with-tools "Use function calling for: What's the weather like in New York?" '("get_weather" "calculate"))
get_weather called with args: ((location . New York))
"Weather in New York: Sunny, 72°F"
|#
```

Sample output:

TBD



