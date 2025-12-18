(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-base-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defvar *model* "gemini-3-flash-preview") ;; model used for all use cases in this file.

(defun escape-json (json-string)
  (with-output-to-string (s)
    (loop for char across json-string
          do (case char
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (t (write-char char s))))))

(defun run-curl-command (curl-command)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program curl-command
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        output
        (error "Curl command failed: ~A~%Error: ~A" curl-command error-output))))

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the specified model.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
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
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
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
  
;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")

(defun count-tokens (prompt &optional (model-id *model*))
  "Counts the number of tokens for a given prompt and model.
   Uses *model* defined at top of this file as default.
   PROMPT: The text prompt to count tokens for.
   MODEL-ID: Optional. The ID of the model to use.
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
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; cl-json by default uses :UPCASE for keys, so :TOTAL-TOKENS should be correct
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S" decoded-response)))))

;; (gemini:count-tokens "In one sentence, explain how AI works to a child.")

(defun run-tests ()
  "Runs tests for generate and count-tokens functions."
  (let* ((prompt "In one sentence, explain how AI works to a child.")
         (generated-text (generate prompt))
         (token-count (count-tokens prompt)))
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
       (let ((gemini-response (gemini:generate
                (concatenate 'string *chat-history* "\nUser: " user-prompt))))
         (princ gemini-response)
         (finish-output)
         (setf *chat-history*
               (concatenate 'string "User: " user-prompt "\n" "Gemini: " gemini-response
                                  "\n" *chat-history* "\n\n")))))))

;; (gemini::chat)

(defun generate-with-search (prompt &optional (model-id *model*))
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (setf (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "google_search" tool)
                        (make-hash-table :test 'equal))
                  tool)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
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

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")

(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
  (let* ((payload (make-hash-table :test 'equal)))
    ;; Payload construction same as previous example):
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (setf (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "google_search" tool)
                        (make-hash-table :test 'equal))
                  tool)))
    
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           ;; 1. Extract Content Text
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts))
           (text-pair (assoc :TEXT first-part))
           (text (cdr text-pair))
           ;; 2. Extract Grounding Metadata
           (metadata-pair (assoc :GROUNDING-METADATA candidate))
           (metadata (cdr metadata-pair))
           (chunks-pair (assoc :GROUNDING-CHUNKS metadata))
           (chunks (cdr chunks-pair))
           ;; 3. Loop through chunks to find Web sources
           (citations (loop for chunk in chunks
                            for web-data-pair = (assoc :WEB chunk)
                            for web-data = (cdr web-data-pair)
                            when web-data
                            collect (cons (cdr (assoc :TITLE web-data))
                                          (cdr (assoc :URI web-data))))))
      ;; Return both text and citations
      (values text citations))))

#|
(multiple-value-bind (response sources)
    (gemini:generate-with-search-and-citations "Who won the Super Bowl in 2024?")
  (format t "Answer: ~a~%~%Sources:~%" response)
  (loop for (title . url) in sources
        do (format t "- ~a: ~a~%" title url)))
|#