(in-package #:gemini)

(defvar *google-api-key* nil
  "Google API key. When NIL, the GOOGLE_API_KEY environment variable is read
   at call time (so a key set after this library is loaded is still found).")

(defvar *model* "gemini-3-flash-preview") ;; model used in this file.

(defun google-api-key ()
  "Return the Google API key, signaling an error if none is configured."
  (or *google-api-key*
      (uiop:getenv "GOOGLE_API_KEY")
      (error "Set the GOOGLE_API_KEY environment variable (or gemini::*google-api-key*)")))

(defun %post-json (url headers payload-hash)
  "Helper function to perform an HTTP POST request with a JSON payload using Dexador."
  (let ((payload-json (cl-json:encode-json-to-string payload-hash)))
    (dex:post url :headers (append headers '(("Accept-Encoding" . "identity")))
                 :content payload-json)))

;;; ---- generateContent API ----

(defun %generate-content (prompt model-id &key google-search-p)
  "POST PROMPT to the generateContent API for MODEL-ID, optionally enabling
   Google Search grounding. Returns the first candidate of the decoded response."
  (let* ((payload (make-hash-table :test 'equal))
         (part-ht (make-hash-table :test 'equal))
         (content-ht (make-hash-table :test 'equal)))
    (setf (gethash "text" part-ht) prompt)
    (setf (gethash "parts" content-ht) (list part-ht))
    (setf (gethash "contents" payload) (list content-ht))
    (when google-search-p
      (let ((google-search-tool (make-hash-table :test 'equal)))
        (setf (gethash "google_search" google-search-tool) (make-hash-table :test 'equal))
        (setf (gethash "tools" payload) (list google-search-tool))))
    (let* ((url (concatenate 'string
                             "https://generativelanguage.googleapis.com/v1beta/models/"
                             model-id ":generateContent"))
           (headers (list '("Content-Type" . "application/json")
                          (cons "X-goog-api-key" (google-api-key))))
           (response-string (%post-json url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string)))
      (first (cdr (assoc :CANDIDATES decoded-response))))))

(defun %candidate-text (candidate)
  "Return the text of the first part of a generateContent CANDIDATE."
  (let* ((content (cdr (assoc :CONTENT candidate)))
         (parts (cdr (assoc :PARTS content))))
    (cdr (assoc :TEXT (first parts)))))

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the Gemini generateContent API.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the generated text as a string."
  (%candidate-text (%generate-content prompt model-id)))

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
                          (cons "x-goog-api-key" (google-api-key))))
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

(defun chat ()
  "Simple REPL chat; the conversation so far is sent with each new prompt."
  (let ((chat-history ""))
   (loop
     (princ "Enter a prompt: ")
     (finish-output)
     (let ((user-prompt (read-line)))
       (when (member user-prompt '("exit" "quit") :test #'string-equal)
         (return))
       (let ((gemini-response
               (or (generate (concatenate 'string chat-history "User: " user-prompt))
                   "")))
         (format t "~A~%" gemini-response)
         (finish-output)
         ;; append so the history stays in chronological order:
         (setf chat-history
               (concatenate 'string chat-history
                            "User: " user-prompt (string #\Newline)
                            "Gemini: " gemini-response (string #\Newline))))))))

;; (gemini::chat)

(defun generate-with-search (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding via the generateContent API."
  (%candidate-text (%generate-content prompt model-id :google-search-p t)))

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today?")
;; (gemini:generate-with-search "What sci-fi movies are playing at Harkins 16 in Flagstaff today? Return data only as JSON with keys being movie titles and show times as a list of strings.")


(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding and returns citations via the generateContent API.
   Returns two values: the response text and a list of (title . url) citation pairs."
  (let* ((first-candidate (%generate-content prompt model-id :google-search-p t))
         (text (%candidate-text first-candidate))
         (grounding-metadata (cdr (assoc :GROUNDING-METADATA first-candidate)))
         (grounding-chunks (cdr (assoc :GROUNDING-CHUNKS grounding-metadata)))
         (citations (loop for chunk in grounding-chunks
                          for web = (cdr (assoc :WEB chunk))
                          when web
                          collect (cons (cdr (assoc :TITLE web))
                                        (cdr (assoc :URI web))))))
    (values text citations)))

#|
(multiple-value-bind (response sources)
    (gemini:generate-with-search-and-citations "Who won the Super Bowl in 2024?")
  (format t "Answer: ~a~%~%Sources:~%" response)
  (loop for (title . url) in sources
        do (format t "- ~a: ~a~%" title url)))
|#
