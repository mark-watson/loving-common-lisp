(in-package #:openai)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defvar *model-host* "https://api.openai.com/v1/chat/completions")
;; use gpt-4o for very good results, or gpt-4o-mini to save abt 20x on costs, with similar results:
(defvar *model* "gpt-4o-mini")

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

(defun handle-tool-call (tool-call)
  (let* ((func-dict (cdr (assoc :function tool-call)))
         (name (cdr (assoc :name func-dict)))
         (args-string (cdr (assoc :arguments func-dict)))
         (args (when args-string (cl-json:decode-json-from-string args-string)))
         (func (openai-function-func (gethash name *available-functions*)))
         (id (cdr (assoc :id tool-call))))
    (if func
        (let ((result (apply func (mapcar #'cdr args))))
          (list id name (format nil "~a" result)))
        (list id name (format nil "Error: Unknown function ~a" name)))))

(defun call-api (curl-command)
  (format t "~%~a~%" curl-command)
  (let ((response (uiop:run-program curl-command :output :string :error-output *standard-output*)))
    (format t "~%~a~%" response)
    (json:decode-json-from-string response)))

(defun get-tool-defs (tool-names)
  (mapcar (lambda (name)
            (let ((func (gethash name *available-functions*)))
              (list (cons :type "function")
                    (cons :function
                          (list (cons :name (openai-function-name func))
                                (cons :description (openai-function-description func))
                                (cons :parameters (openai-function-parameters func)))))))
          tool-names))

(defun prepare-request (messages max-tokens tool-defs)
  (let ((data (list (cons :model *model*)
                    (cons :messages messages)
                    (cons :max_tokens max-tokens))))
    (when tool-defs
      (push (cons :tools tool-defs) data))
    (cl-json:encode-json-to-string (nreverse data))))

(defun chat-once (messages max-tokens tool-names)
  (let* ((tool-defs (when tool-names (get-tool-defs tool-names)))
         (request-body (prepare-request messages max-tokens tool-defs))
         (fixed (substitute-subseq request-body ":null" ":false"))
         (escaped (escape-json fixed))
         (curl (format nil "curl ~a -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~a\" -d \"~a\""
                       *model-host* (uiop:getenv "OPENAI_API_KEY") escaped)))
    (call-api curl)))

(defun agent-completions (starter-text max-tokens &optional tool-names (max-iter 5))
  (let ((messages (list (list (cons :role "user") (cons :content starter-text)))))
    (loop for iter from 0 below max-iter
          do (let* ((json-response (chat-once messages max-tokens tool-names))
                    (choices (cdr (assoc :choices json-response)))
                    (first-choice (first choices))
                    (message (cdr (assoc :message first-choice)))
                    (content (cdr (assoc :content message)))
                    (tool-calls (cdr (assoc :tool-calls message))))
               (format t "~%Message: ~a~%" message)
               (setf messages (append messages (list message)))
               (if tool-calls
                   (let ((tool-results (mapcar #'handle-tool-call tool-calls)))
                     (setf messages (append messages
                                            (mapcar (lambda (res)
                                                      (let ((id (first res)) (name (second res)) (cont (third res)))
                                                        (list (cons :role "tool")
                                                              (cons :tool-call-id id)
                                                              (cons :name name)
                                                              (cons :content cont))))
                                                    tool-results))))
                   (return (or content "No content"))))
          finally (return "Max iterations exceeded"))))

(defun summarize (some-text max-tokens)
  (agent-completions (format nil "Summarize: ~a" some-text) max-tokens))

(defun answer-question (question-text max-tokens)
  (agent-completions question-text max-tokens))

(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((curl-command
          (concatenate 'string
                       "curl https://api.openai.com/v1/embeddings "
                       " -H \"Content-Type: application/json\""
                       " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                       " -d '{\"input\": \"" text 
                       "\", \"model\": \"text-embedding-3-small\"}'"))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

(defun dot-product-recursive (a b)
  "Calculate dot product recursively"
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively"
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))

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
 #'get_weather)

(defun calculate_math (expression)
  (handler-case
      (format nil "~a" (eval (read-from-string expression)))
    (error (e) (format nil "Error: ~a" e))))

(register-function
 "calculate_math"
 "Evaluate a mathematical expression"
 (list (cons :type "object")
       (cons :properties (list (cons :expression (list (cons :type "string")
                                                       (cons :description "The math expression to evaluate, e.g., '2 + 3 * 4'")))))
       (cons :required '("expression")))
 #'calculate_math)

(defun list_current_directory ()
  (format nil "~{~a~^, ~}" (mapcar #'uiop::file-namestring (uiop:directory-files (uiop:getcwd)))))

(register-function
 "list_current_directory"
 "Get the list of files in the current working directory"
 (list (cons :type "object")
       (cons :properties nil)
       (cons :required nil))
 #'list_current_directory)

(defun get_file_content (filename)
  (handler-case
      (uiop:read-file-string (merge-pathnames filename (uiop:getcwd)))
    (error (e) (format nil "Error: ~a" e))))

(register-function
 "get_file_content"
 "Get the contents of a file in the current directory"
 (list (cons :type "object")
       (cons :properties (list (cons :filename (list (cons :type "string")
                                                     (cons :description "The filename to read")))))
       (cons :required '("filename")))
 #'get_file_content)

(agent-completions "Use function calling for: What's the weather like in New York?" 1000 '("get_weather"))

#|
;; Example calls:

(print (agent-completions "The President went to Congress" 20))
(print (summarize "Jupiter is the fifth planet from the Sun..." 30))
(print (answer-question "Where were the 1992 Olympics held?" 60))
(print (answer-question "Where is the Valley of Kings?" 60))
(print (answer-question "Mary is 30 years old and Bob is 25. Who is older?" 60))
(print (agent-completions "Use function calling for: What's the weather like in New York?" 100 '("get_weather" "calculate_math")))
(print (agent-completions "Use function calling to list files in directory" 100 '("list_current_directory")))
(print (agent-completions "Use function calling to get contents of openai.lisp" 100 '("get_file_content")))
|#

