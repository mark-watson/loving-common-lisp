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
   FUNCTIONS is an optional list of registered function names to make available.
   Runs the complete litelm tool-use loop: every tool call the model requests is
   dispatched, each result is appended to the conversation as a :tool message,
   and the model is called again until it returns a plain final answer.
   Returns the final answer string."
  (let* ((full-model (ensure-model-name *tool-model-name*))
         (tool-defs (when functions
                      (%convert-to-litelm-tools functions)))
         (messages (list (list :user starter-text))))
    (loop
      (let* ((resp (litelm:completion full-model
                                      :messages messages
                                      :tools tool-defs
                                      :api-base *model-host*))
             (content (litelm:response-content resp))
             (tool-calls (litelm:response-tool-calls resp)))
        (format t "Raw response: ~s~%" (litelm:response-raw resp))
        (if tool-calls
            (progn
              (format t "~%Model requested ~a tool call(s).~%" (length tool-calls))
              ;; Record the assistant turn that asked for the tool calls.
              (setf messages
                    (append messages
                            (list (list :assistant content :tool-calls tool-calls))))
              ;; Execute every requested call and feed each result back.
              (dolist (tc tool-calls)
                (let ((result (handle-tool-function-call tc)))
                  (format t "  Tool ~a completed.~%" (getf tc :name))
                  (setf messages
                        (append messages
                                (list (list :tool (format nil "~a" result)
                                            :tool-call-id (getf tc :id))))))))
            ;; No tool calls - the model produced its final answer.
            (return (or content "No response content")))))))

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (or (cdr (assoc :location args :test #'string-equal))
                      (cdr (assoc "location" args :test #'string-equal)))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

;;; The calculate tool evaluates ordinary infix arithmetic ("2 + 2",
;;; "(3 + 4) * 2") written by the LLM. That text is untrusted, so the path from
;;; string to number is deliberately narrow:
;;;   1. Tokens are restricted to ASCII digits, "." and the operators + - * / ( ).
;;;   2. Number literals are read with *READ-EVAL* bound to NIL, which disables
;;;      #. read-time evaluation.
;;;   3. The parsed form is walked by %EVALUATE-MATH, which understands only
;;;      numbers and the four operators. No symbol is ever resolved and EVAL is
;;;      never called, so LLM-supplied text cannot execute arbitrary Lisp.
;;; Expression length is capped so a hostile or runaway prompt cannot consume
;;; unbounded time or memory building huge bignums and rationals.

(defvar *calculate-max-expression-length* 1000
  "Maximum number of characters accepted by the calculate tool's :expression.")

(defun %ascii-digit-char-p (ch)
  "True if CH is one of the ASCII digits 0-9."
  (char<= #\0 ch #\9))

(defun %tokenize-math (string)
  "Split the infix math STRING into a vector of numbers and operator characters."
  (let ((*read-eval* nil)
        (tokens '())
        (number (make-string-output-stream)))
    (labels ((flush-number ()
               (let ((text (get-output-stream-string number)))
                 (when (plusp (length text))
                   (push (read-from-string text) tokens)))))
      (loop for ch across string do
        (cond ((or (%ascii-digit-char-p ch) (char= ch #\.))
               (write-char ch number))
              ((find ch "+-*/()") (flush-number) (push ch tokens))
              (t (flush-number))))
      (flush-number))
    (coerce (nreverse tokens) 'vector)))

(defun %parse-math (tokens)
  "Parse the infix TOKENS into a prefix Lisp form. Grammar:
     expression := term (('+' | '-') term)*
     term       := factor (('*' | '/') factor)*
     factor     := number | '(' expression ')' | '-' factor"
  (let ((pos 0))
    (labels ((peek () (when (< pos (length tokens)) (aref tokens pos)))
             (take () (prog1 (peek) (incf pos)))
             (expression ()
               (let ((left (term)))
                 (loop for op = (peek)
                       while (member op '(#\+ #\-))
                       do (take)
                          (setf left (list (if (char= op #\+) '+ '-)
                                           left (term)))
                       finally (return left))))
             (term ()
               (let ((left (factor)))
                 (loop for op = (peek)
                       while (member op '(#\* #\/))
                       do (take)
                          (setf left (list (if (char= op #\*) '* '/)
                                           left (factor)))
                       finally (return left))))
             (factor ()
               (let ((token (take)))
                 (cond ((null token) (error "Unexpected end of expression"))
                       ((numberp token) token)
                       ((char= token #\() (prog1 (expression)
                                            (unless (eql (take) #\))
                                              (error "Missing close parenthesis"))))
                       ((char= token #\-) (list '- (factor)))
                       (t (error "Unexpected token ~s" token))))))
      (let ((form (expression)))
        (when (peek)
          (error "Unexpected trailing input"))
        form))))

(defun %evaluate-math (form)
  "Evaluate a parsed arithmetic FORM of numbers and the operators + - * /.
Returns a number. Signals an error for any other shape, so a malformed tree can
never reach the Lisp evaluator."
  (cond
    ((numberp form) form)
    ;; Unary minus, produced by FACTOR for input such as "-3 + 10".
    ((and (consp form) (eq (car form) '-) (null (cddr form)))
     (- (%evaluate-math (second form))))
    ;; Binary operation: exactly three elements, operator restricted to + - * /.
    ((and (consp form) (member (car form) '(+ - * /)) (null (cdddr form)))
     (let ((left (%evaluate-math (second form)))
           (right (%evaluate-math (third form))))
       (ecase (car form)
         (+ (+ left right))
         (- (- left right))
         (* (* left right))
         (/ (/ left right)))))
    (t (error "Refusing to evaluate unsafe expression form: ~s" form))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (format t "calculate called with args: ~a~%" args)
  (let ((expression (or (cdr (assoc :expression args :test #'string-equal))
                        (cdr (assoc "expression" args :test #'string-equal)))))
    (if expression
        (handler-case
            (progn
              (when (> (length expression) *calculate-max-expression-length*)
                (error "Expression too long (~a characters, limit is ~a)"
                       (length expression) *calculate-max-expression-length*))
              (format nil "Result: ~a"
                      (%evaluate-math (%parse-math (%tokenize-math expression)))))
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
