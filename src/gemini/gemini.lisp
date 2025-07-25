(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-base-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun generate (model-id prompt)
  "Generates text from a given prompt using the specified model.
   MODEL-ID: The ID of the model to use.
   PROMPT: The text prompt to generate content from.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts))
           (text-pair (assoc :TEXT first-part))
           (text (cdr text-pair)))
       text)))
  
;; (gemini:generate "gemini-2.0-flash" "In one sentence, explain how AI works to a child.")
;; (gemini:generate "gemini-2.5-flash-preview-05-20" "Write a short, four-line poem about coding in Python.")

(defun count-tokens (model-id prompt)
  "Counts the number of tokens for a given prompt and model.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
PROMPT: The text prompt to count tokens for.
Returns the total token count as an integer."
  (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":countTokens"))
         (payload (make-hash-table :test 'equal)))
    ;; Construct payload similar to generate function
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; cl-json by default uses :UPCASE for keys, so :TOTAL-TOKENS should be correct
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S" decoded-response)))))

;; (gemini:count-tokens "gemini-2.0-flash" "In one sentence, explain how AI works to a child.")

(defun run-tests ()
  "Runs tests for generate and count-tokens functions."
  (let* ((model-id "gemini-2.0-flash")
         (prompt "In one sentence, explain how AI works to a child.")
         (generated-text (generate model-id prompt))
         (token-count (count-tokens model-id prompt)))
    (format t "Generated Text: ~A~%Token Count: ~A~%" generated-text token-count)))

;; Running the test
;; (gemini::run-tests)

(defparameter *chat-history* '())

(defun chat ()
  (let ((*chat-history* ""))
   (loop
     (princ "Enter a prompt: ")
     (finish-output)
     (let ((user-prompt (read-line)))
       (princ user-prompt)
       (finish-output)
       (let ((gemini-response (gemini:generate "gemini-2.0-flash"
                (concatenate 'string *chat-history* "\nUser: " user-prompt))))
         (princ gemini-response)
         (finish-output)
         (setf *chat-history*
               (concatenate 'string "User: " user-prompt "\n" "Gemini: " gemini-response
                                  "\n" *chat-history* "\n\n")))))))

;; (gemini::chat)