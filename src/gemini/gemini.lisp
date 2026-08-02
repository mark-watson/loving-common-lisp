(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *interactions-api-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")

(defvar *model* "gemini-3-flash-preview") ;; model used in this file.

(defun %post-json (url headers payload-hash)
  "Helper function to perform an HTTP POST request with a JSON payload using Dexador."
  (let ((payload-json (cl-json:encode-json-to-string payload-hash)))
    (dex:post url :headers (append headers '(("Accept-Encoding" . "identity")))
                 :content payload-json)))

;;; ---- generateContent API ----

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the Gemini generateContent API.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal))
         (part-ht (make-hash-table :test 'equal))
         (content-ht (make-hash-table :test 'equal)))
    (setf (gethash "text" part-ht) prompt)
    (setf (gethash "parts" content-ht) (list part-ht))
    (setf (gethash "contents" payload) (list content-ht))
    (let* ((url (concatenate 'string
                             "https://generativelanguage.googleapis.com/v1beta/models/"
                             model-id ":generateContent"))
           (headers (list '("Content-Type" . "application/json")
                          (cons "X-goog-api-key" *google-api-key*)))
           (response-string (%post-json url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates (cdr (assoc :CANDIDATES decoded-response)))
           (first-candidate (first candidates))
           (content (cdr (assoc :CONTENT first-candidate)))
           (parts (cdr (assoc :PARTS content)))
           (first-part (first parts)))
      (cdr (assoc :TEXT first-part)))))
  
;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")

(defun count-tokens (prompt &optional (model-id *model*))
  "Counts the number of tokens for a given prompt and model.
   Uses *model* defined at top of this file as default.
   PROMPT: The text prompt to count tokens for.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the total token count as an integer."
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
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)))
           (response-string (%post-json api-url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error
           "Could not retrieve token count from API response: ~S"
           decoded-response)))))

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
       (when (member user-prompt '("exit" "quit") :test #'string-equal)
         (return))
       (let ((gemini-response (gemini:generate
                (concatenate 'string *chat-history* "\nUser: " user-prompt))))
         (format t "~A~%" gemini-response)
         (finish-output)
         (setf *chat-history*
               (concatenate 'string "User: " user-prompt "\n" "Gemini: "
			    gemini-response
                            "\n" *chat-history* "\n\n")))))))

;; (gemini::chat)

(defun generate-with-search (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding via the generateContent API."
  (let* ((payload (make-hash-table :test 'equal))
         (part-ht (make-hash-table :test 'equal))
         (content-ht (make-hash-table :test 'equal))
         (google-search-tool (make-hash-table :test 'equal)))
    (setf (gethash "text" part-ht) prompt)
    (setf (gethash "parts" content-ht) (list part-ht))
    (setf (gethash "contents" payload) (list content-ht))
    (setf (gethash "google_search" google-search-tool) (make-hash-table :test 'equal))
    (setf (gethash "tools" payload) (list google-search-tool))
    (let* ((url (concatenate 'string
                             "https://generativelanguage.googleapis.com/v1beta/models/"
                             model-id ":generateContent"))
           (headers (list '("Content-Type" . "application/json")
                          (cons "X-goog-api-key" *google-api-key*)))
           (response-string (%post-json url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates (cdr (assoc :CANDIDATES decoded-response)))
           (first-candidate (first candidates))
           (content (cdr (assoc :CONTENT first-candidate)))
           (parts (cdr (assoc :PARTS content)))
           (first-part (first parts)))
      (cdr (assoc :TEXT first-part)))))

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today? Return data only as JSON with keys being movie titles and show times as a list of strings.")


(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding and returns citations via the generateContent API.
   Returns two values: the response text and a list of (title . url) citation pairs."
  (let* ((payload (make-hash-table :test 'equal))
         (part-ht (make-hash-table :test 'equal))
         (content-ht (make-hash-table :test 'equal))
         (google-search-tool (make-hash-table :test 'equal)))
    (setf (gethash "text" part-ht) prompt)
    (setf (gethash "parts" content-ht) (list part-ht))
    (setf (gethash "contents" payload) (list content-ht))
    (setf (gethash "google_search" google-search-tool) (make-hash-table :test 'equal))
    (setf (gethash "tools" payload) (list google-search-tool))
    (let* ((url (concatenate 'string
                             "https://generativelanguage.googleapis.com/v1beta/models/"
                             model-id ":generateContent"))
           (headers (list '("Content-Type" . "application/json")
                          (cons "X-goog-api-key" *google-api-key*)))
           (response-string (%post-json url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates (cdr (assoc :CANDIDATES decoded-response)))
           (first-candidate (first candidates))
           (content (cdr (assoc :CONTENT first-candidate)))
           (parts (cdr (assoc :PARTS content)))
           (first-part (first parts))
           (text (cdr (assoc :TEXT first-part)))
           (grounding-metadata (cdr (assoc :GROUNDINGMETADATA first-candidate)))
           (grounding-chunks (cdr (assoc :GROUNDINGCHUNKS grounding-metadata)))
           (citations (loop for chunk in grounding-chunks
                            for web = (cdr (assoc :WEB chunk))
                            when web
                            collect (cons (cdr (assoc :TITLE web))
                                          (cdr (assoc :URI web))))))
      (values text citations))))

#|
(multiple-value-bind (response sources)
    (gemini:generate-with-search-and-citations "Who won the Super Bowl in 2024?")
  (format t "Answer: ~a~%~%Sources:~%" response)
  (loop for (title . url) in sources
        do (format t "- ~a: ~a~%" title url)))
|#
