;;;; Moonshot AI API Example in Common Lisp
;;;;
;;;; This script demonstrates how to call the Moonshot AI Chat Completions API
;;;; using Common Lisp. 
;;;;
;;;; Requirements:
;;;; - cl-json: For encoding Lisp data to JSON and decoding JSON responses.
;;;; - uiop: For accessing environment variables (part of ASDF/standard).
;;;; - alexandria: For various utility functions (common in Lisp projects).

;; Load necessary libraries using Quicklisp.
;; Ensure alexandria is included as it's used for hash-table utilities.
(ql:quickload '("cl-json" "uiop" "alexandria"))

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

(defun get-kimi-chat-completion (user-prompt)
  "Sends a prompt to the Moonshot AI (Kimi) chat completion API and returns the content of the response.

  Args:
    user-prompt: A string containing the user's message.

  Returns:
    A string with the assistant's reply, or NIL on error."

  (let* ((api-key (uiop:getenv "MOONSHOT_API_KEY"))
         (base-url "https://api.moonshot.ai/v1/chat/completions")
         ;; Construct the payload as an association list (alist).
         ;; cl-json:encode-json can convert alists directly to JSON objects.
         ;; We use a vector for the 'messages' array.
         (payload
           `((:model . "kimi-k2-0711-preview")
             (:messages . ,(vector
                            `((:role . "system")
                              (:content . "You are Kimi, an AI assistant provided by Moonshot AI. You are proficient in English conversations. You provide users with safe, helpful, and accurate answers."))
                            `((:role . "user")
                              (:content . ,user-prompt))))
             (:temperature . 0.3))))

    ;; Validate that the API key is present.
    (unless (and api-key (not (string= api-key "")))
      (error "MOONSHOT_API_KEY environment variable is not set."))

    (handler-case
        (let* (;; Encode the payload alist into a JSON string.
               (json-payload (json:encode-json-to-string payload))
               
               (escaped-json (escape-json json-payload))
               (curl-cmd
                (format
                 nil
                 "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
                 base-url api-key escaped-json))
               (response-body (run-curl-command curl-cmd))
               
               ;; Decode the JSON response string from the server into a Lisp alist.
               (parsed-response (json:decode-json-from-string response-body))
               
               ;; Extract the assistant's message content from the nested structure.
               ;; The expected structure is:
               ;; (:CHOICES ((:MESSAGE (:CONTENT . "...") ...)))
               (choices (cdr (assoc :choices parsed-response)))
               (first-choice (first choices))
               (message (cdr (assoc :message first-choice)))
               (content (cdr (assoc :content message))))
          content)
      
      ;; Handle any other unexpected errors.
      (error (e)
        (format *error-output* "An unexpected error occurred: ~A~%" e)
        nil))))

;; Example usage:
;; Run this script with: MOONSHOT_API_KEY=your_key_here sbcl --load generate.lisp
(let ((response (get-kimi-chat-completion "What is 1 + 11 + 111?")))
  (if response
      (format t "Kimi: ~A~%" response)
      (format t "Failed to get a response from Kimi.~%")))
