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


(defun handle-function-call (function-call)
  ;; function-call looks like: \
  ;;  ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))
  (format t "~% ** handle-function-call (DUMMY) fucntion-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (moonshot-function-func (gethash name *available-functions*))))
    (format t "~% handle-function-call name: ~A" name)
    (format t "~% handle-function-call args-string: ~A" args-string)
    (format t "~% handle-function-call args: ~A" args)
    (format t "~% handle-function-call func: ~A" func)
    (if (not (null func))
	(let ()
          (format t "~%Calling function ~a called with args: ~a~%" name args)
	  (let ((f-val (apply func (mapcar #'cdr args))))
	    (format t "~%Return value from func ~A is ~A~%" name f-val)
	    f-val))
        (error "Unknown function: ~a" name))))

(defun moonshot-helper (curl-command)
  (let ((response (uiop:run-program curl-command
                                    :output :string
                                    :error-output :string)))
    (terpri)
    (princ response)
    (terpri)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
	(format t "~% json-as-list: ~A~%" json-as-list)
	(format t "~% choices: ~A~%" choices)
	(format t "~% first-choice: ~A~%" first-choice)
	(format t "~% message: ~A~%" message)
	(format t "~% function-call: ~A~%" function-call)
	(format t "~% content: ~A~%" content)
        (if function-call
            (handle-function-call function-call)
            (or content "No response content"))))))


(defun completion (starter-text &optional functions)
  (let* ((function-defs
	   (when functions
             (mapcar (lambda (f)
                       (let ((func (gethash f *available-functions*)))
                         (list (cons :name (moonshot-function-name func))
                               (cons :description (moonshot-function-description func))
                               (cons :parameters (moonshot-function-parameters func)))))
                     functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))))
         (data (if function-defs
                   (append
		    base-data
		    (list (cons :functions function-defs)))
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
;; Example calls:

(print (completion "The President went to Congress"))
(print (completion "Where were the 1992 Olympics held?"))
(print (completion "Where is the Valley of Kings?"))
(print (completion "Mary is 30 years old and Bob is 25. Who is older?"))
(print (completion "Use function calling for: What's the weather like in New York?" '("get_weather")))
|#

