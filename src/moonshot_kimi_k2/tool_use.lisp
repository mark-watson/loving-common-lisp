;; define the environment variable "MOONSHOT_API_KEY" with the value of your MOONSHOT AI API key

(defvar *model-host* "https://api.moonshot.ai/v1/chat/completions")
(defvar *model* "kimi-k2-0711-preview")

;; Hash table to store available functions for tool calling
(defvar *available-functions* (make-hash-table :test 'equal))

(ql:quickload '("drakma" "cl-json" "uiop"))

(defstruct moonshot-function
  name
  description
  parameters
  func)

(defun register-function (name description parameters fn)
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-moonshot-function
         :name name
         :description description
         :parameters parameters
	 :func fn)))

; #S(moonshot-function
;    :name get_weather
;    :description Get current weather for a location
;    :parameters ((type . object)
;                 (properties
;                  (location (type . string)
;                            (description . The city name)))
;                 (required location)))

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

(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
         (if (char= ch #\")
             (write-string "\\\"" out)
             (write-char ch out)))))


(defun json-value (alist &rest keys)
  (loop for key in keys
        for pair = (assoc key alist)
        when pair return (cdr pair)))

(defun json-has-key-p (alist &rest keys)
  (loop for key in keys
        when (assoc key alist) return t))

(defun normalize-tool-calls (tool-calls)
  (cond
    ((null tool-calls) nil)
    ((vectorp tool-calls) (coerce tool-calls 'list))
    ;; Already a list of tool call objects
    ((and (listp tool-calls)
          (every #'consp tool-calls)
          (json-has-key-p (car tool-calls) :type 'type)) tool-calls)
    ;; Single tool call object represented as an alist
    ((json-has-key-p tool-calls :type 'type) (list tool-calls))
    (t (list tool-calls))))

(defun argument-values-from-json (args)
  (cond
    ((null args) nil)
    ((hash-table-p args)
     (loop for value being the hash-values of args collect value))
    ((and (listp args) (every #'consp args))
     (mapcar #'cdr args))
    ((listp args) args)
    (t (list args))))

(defun handle-tool-calls (tool-calls)
  ;; tool-calls looks like a list of alists:
  ;;  ((:type . "function")
  ;;   (:function ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))))
  (format t "~% ** handle-tool-calls data: ~A~%" tool-calls)
  (let ((calls (normalize-tool-calls tool-calls))
        (final-result nil))
    (dolist (tool-call calls final-result)
      (let* ((function-payload (or (json-value tool-call :function 'function)
                                   tool-call))
             (name (json-value function-payload :name 'name))
             (args-string (json-value function-payload :arguments 'arguments))
             (args (and args-string (cl-json:decode-json-from-string args-string)))
             (func (and name (moonshot-function-func (gethash name *available-functions*)))))
        (unless func
          (error "Unknown function: ~a" name))
        (format t "~%Calling function ~a with args: ~a~%" name args)
        (setf final-result (apply func (argument-values-from-json args)))
        (format t "~%Return value from func ~A is ~A~%" name final-result)))))

(defun moonshot-helper (curl-command)
  (let ((response (uiop:run-program curl-command
                                    :output :string
                                    :error-output :string)))
    (terpri)
    (princ response)
    (terpri)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (json-value json-as-list :choices 'choices))
             (first-choice (cond
                             ((vectorp choices)
                              (and (> (length choices) 0) (aref choices 0)))
                             ((listp choices) (car choices))
                             (t nil)))
             (message (and first-choice (json-value first-choice :message 'message)))
             (tool-calls (and message
                              (json-value message
                                          :tool_calls 'tool_calls :tool-calls 'tool-calls
                                          :tool--calls 'tool--calls)))
             (content (and message (json-value message :content 'content))))
	(format t "~% json-as-list: ~A~%" json-as-list)
	(format t "~% choices: ~A~%" choices)
	(format t "~% first-choice: ~A~%" first-choice)
	(format t "~% message: ~A~%" message)
	(format t "~% tool-calls: ~A~%" tool-calls)
	(format t "~% content: ~A~%" content)
        (if tool-calls
            (handle-tool-calls tool-calls)
            (or content "No response content"))))))


(defun completion (starter-text &optional functions)
  (let* ((function-defs
	   (when functions
             (mapcar (lambda (f)
                       (let ((func (gethash f *available-functions*)))
                         (unless func
                           (error "Function ~A is not registered." f))
                         (list (cons :type "function")
                               (cons :function
                                     (list (cons :name (moonshot-function-name func))
                                           (cons :description (moonshot-function-description func))
                                           (cons :parameters (moonshot-function-parameters func)))))))
                     functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))))
         (data (if function-defs
                   (append
		    base-data
		    (list (cons :tools function-defs)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data
	   (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json-data))
         (curl-command
           (format
	    nil
	    "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
            *model-host*
            (uiop:getenv "MOONSHOT_API_KEY")
            escaped-json)))
    (moonshot-helper curl-command)))


;;; Sample registrations for functions used in tool calling

(defun get_weather (location)
  (if (equal location "New York")
      77.0
      65.0))

(register-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :type "object")
       (cons
	:properties
	(list
	 (cons :location
	       (list (cons :type "string")
                     (cons :description "The city name")))))
       (cons :required '("location")))
 #'get_weather)


#|
;; Example calls:			;
					;
(print (completion "The President went to Congress")) ;
(print (completion "Where were the 1992 Olympics held?")) ;
(print (completion "Where is the Valley of Kings?")) ;
(print (completion "Mary is 30 years old and Bob is 25. Who is older?")) ;
(print (completion "Use function calling for: What's the weather like in New York?" '("get_weather")))
					;
|#
