(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *interactions-api-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")

(defvar *model* "gemini-3-flash-preview") ;; model used in this file.

(defun run-curl-command (curl-command
                         &optional temp-file)
  "Run CURL-COMMAND via uiop, cleaning up TEMP-FILE
   (if any) afterward."
  (unwind-protect
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program curl-command
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (if (zerop exit-code)
          output
          (error "Curl command failed: ~A~%Error: ~A"
                 curl-command error-output)))
    (when (and temp-file (probe-file temp-file))
      (delete-file temp-file))))

;;; ---- Interactions API helpers ----

(defun %interactions-curl-cmd (payload)
  "Build a curl command for the Interactions API.
   Writes the JSON payload to a temp file and uses
   curl -d @file to avoid all shell quoting issues.
   Returns (VALUES curl-command temp-file-path)."
  (let* ((data (cl-json:encode-json-to-string payload))
         (temp-file (uiop:tmpize-pathname
                     (merge-pathnames
                      "gemini-payload.json"
                      (uiop:temporary-directory))))
         (temp-path (namestring temp-file)))
    (with-open-file (out temp-file
                     :direction :output
                     :if-exists :supersede)
      (write-string data out))
    (values
     (format nil
       "curl -s -X POST ~A ~
        -H \"Content-Type: application/json\" ~
        -H \"x-goog-api-key: ~A\" ~
        -H \"Api-Revision: 2026-05-20\" ~
        -d @~A"
       *interactions-api-url*
       *google-api-key*
       temp-path)
     temp-path)))

(defun %extract-text-from-steps (decoded-response)
  "Extract the text from the last model_output step in an Interactions API response.
   Response format: {\"steps\": [{\"type\": \"model_output\", \"content\": [{\"type\": \"text\", \"text\": \"...\"}]}]}"
  (let* ((steps (cdr (assoc :STEPS decoded-response))))
    (loop for step in (reverse steps)
          when (string-equal (cdr (assoc :TYPE step)) "model_output")
          return (let* ((content (cdr (assoc :CONTENT step)))
                        (first-content (first content)))
                   (cdr (assoc :TEXT first-content))))))

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the Interactions API.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt)
    (multiple-value-bind (curl-cmd temp-file)
        (%interactions-curl-cmd payload)
      (let* ((response-string (run-curl-command curl-cmd temp-file))
             (decoded-response (cl-json:decode-json-from-string response-string)))
        (%extract-text-from-steps decoded-response)))))
  
;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")

(defun count-tokens (prompt &optional (model-id *model*))
  "Counts the number of tokens for a given prompt and model.
   Uses *model* defined at top of this file as default.
   PROMPT: The text prompt to count tokens for.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the total token count as an integer.
   NOTE: countTokens still uses the generateContent-style endpoint."
  (let* ((api-url (concatenate 'string
                               "https://generativelanguage.googleapis.com/v1beta/models/"
                               model-id ":countTokens"))
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
           (temp-file (uiop:tmpize-pathname
                       (merge-pathnames
                        "gemini-count-tokens.json"
                        (uiop:temporary-directory))))
           (temp-path (namestring temp-file)))
      (with-open-file (out temp-file
                       :direction :output
                       :if-exists :supersede)
        (write-string data out))
      (let* ((curl-cmd
              (format nil
               "curl -s -X POST ~A ~
                -H \"Content-Type: application/json\" ~
                -H \"x-goog-api-key: ~A\" ~
                -d @~A"
               api-url *google-api-key* temp-path))
             (response-string (run-curl-command curl-cmd
                                                temp-file))
             (decoded-response
              (cl-json:decode-json-from-string
               response-string))
             (total-tokens-pair
              (assoc :TOTAL-TOKENS decoded-response)))
        (if total-tokens-pair
            (cdr total-tokens-pair)
            (error
             "Could not retrieve token count ~
              from API response: ~S"
             decoded-response))))))

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
               (concatenate 'string "User: " user-prompt "\n" "Gemini: "
			    gemini-response
                            "\n" *chat-history* "\n\n")))))))

;; (gemini::chat)

(defun generate-with-search (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding via the Interactions API."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (multiple-value-bind (curl-cmd temp-file)
        (%interactions-curl-cmd payload)
      (let* ((response-string (run-curl-command curl-cmd temp-file))
             (decoded-response (cl-json:decode-json-from-string response-string)))
        (%extract-text-from-steps decoded-response)))))

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today? Return data only as JSON with keys being movie titles and show times as a list of strings.")


(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding and returns citations via the Interactions API.
   Returns two values: the response text and a list of (title . url) citation pairs."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (multiple-value-bind (curl-cmd temp-file)
        (%interactions-curl-cmd payload)
      (let* ((response-string (run-curl-command curl-cmd temp-file))
             (decoded-response (cl-json:decode-json-from-string response-string))
           (steps (cdr (assoc :STEPS decoded-response)))
           ;; Extract text from last model_output step
           (text (loop for step in (reverse steps)
                       when (string-equal (cdr (assoc :TYPE step)) "model_output")
                       return (let* ((content (cdr (assoc :CONTENT step)))
                                     (first-content (first content)))
                                (cdr (assoc :TEXT first-content)))))
           ;; Extract citations from url_citation annotations in model_output steps
           (citations
            (loop for step in steps
                  when (string-equal (cdr (assoc :TYPE step)) "model_output")
                  append (loop for content-item in (cdr (assoc :CONTENT step))
                               append (loop for annotation in (cdr (assoc :ANNOTATIONS content-item))
                                            when (string-equal (cdr (assoc :TYPE annotation)) "url_citation")
                                            collect (cons (cdr (assoc :TITLE annotation))
                                                          (cdr (assoc :URL annotation))))))))
      ;; Return both text and citations
      (values text citations)))))

#|
(multiple-value-bind (response sources)
    (gemini:generate-with-search-and-citations "Who won the Super Bowl in 2024?")
  (format t "Answer: ~a~%~%Sources:~%" response)
  (loop for (title . url) in sources
        do (format t "- ~a: ~a~%" title url)))
|#
