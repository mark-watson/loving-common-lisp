(in-package #:openai)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defvar *model-host* "https://api.openai.com/v1/chat/completions")

;; use gpt-5 for better results, but much more expensive:
(defvar *model* "gpt-5-nano")

;; Hash table to store available functions for tool calling
(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct openai-function
  name
  description
  parameters
  func)

(defun register-function (name description parameters fn)
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-openai-function
         :name name
         :description description
         :parameters parameters
	 :func fn)))

; #S(openai-function
;    :name get_weather
;    :description Get current weather for a location
;    :parameters ((type . object)
;                 (properties
;                  (location (type . string)
;                            (description . The city name)))
;                 (required location)))

(defun openai-post (url data)
  (let ((api-key (uiop:getenv "OPENAI_KEY")))
    (unless api-key
      (error "OPENAI_KEY environment variable is not set"))
    (multiple-value-bind (response-body response-status)
        (drakma:http-request
         url
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(concatenate 'string "Bearer " api-key)))
         :content (cl-json:encode-json-to-string data))
      (unless (= response-status 200)
        (error "OpenAI request failed with status ~A: ~A"
               response-status
               (typecase response-body
                 (string response-body)
                 (vector (babel:octets-to-string response-body))
                 (t ""))))
      (typecase response-body
        (string response-body)
        (vector (babel:octets-to-string response-body))
        (t (error "Invalid response body type from OpenAI"))))))

(defun handle-function-call (function-call)
  ;; function-call looks like: ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))
  (format t "~% ** handle-function-call (DUMMY) function-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (openai-function-func (gethash name *available-functions*))))
    (if (not (null func))
	(let ()
          (format t "~%Calling function ~a called with args: ~a~%" name args)
	  (let ((f-val (apply func (mapcar #'cdr args))))
	    (format t "~%Return value from func ~A is ~A~%" name f-val)
	    f-val))
        (error "Unknown function: ~a" name))))

(defun openai-helper (response-str)
  (let* ((json-as-list (json:decode-json-from-string response-str))
         (choices (cdr (assoc :choices json-as-list)))
         (first-choice (car choices))
         (message (cdr (assoc :message first-choice)))
         (function-call (cdr (assoc :function--call message)))
         (content (cdr (assoc :content message))))
    (if function-call
        (handle-function-call function-call)
        (or content "No response content"))))

(defun completions (starter-text &optional functions)
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :name (openai-function-name func))
                                            (cons :description (openai-function-description func))
                                            (cons :parameters (openai-function-parameters func)))))
                                  functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))))
         (data (if function-defs
                   (append base-data (list (cons :functions function-defs)))
                   base-data))
         (response-str (openai-post *model-host* data)))
    (openai-helper response-str)))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))


#|
;;; Sample registrations for functions used in tool calling

(defun get_weather (location)
  (if (equal location "New York")
      77.0
      65.0))

(register-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :type "object")
       (cons :properties (list (cons :location (list (cons :type "string")
                                                     (cons :description "The city name")))))
       (cons :required '("location")))
 #'openai::get_weather)


(openai::completions "Use function calling for: What's the weather like in New York?" 1000 '("get_weather"))
|#


#|
;; Example calls:

(print (completions "Complete the following text: The President went to Congress"))
(print (answer-question "Where were the 1992 Olympics held?"))
(print (answer-question "Where is the Valley of Kings?"))
(print (answer-question "Mary is 30 years old and Bob is 25. Who is older?"))
|#

