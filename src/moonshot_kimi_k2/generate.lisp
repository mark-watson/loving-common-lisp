;;;; Moonshot AI API Example in Common Lisp
;;;;
;;;; This script demonstrates how to call the Moonshot AI Chat Completions API
;;;; using Common Lisp. It requires the Dexador library for HTTP requests
;;;; and cl-json for JSON manipulation.

;; Load necessary libraries using Quicklisp
(ql:quickload '("dexador" "cl-json" "uiop"))

(defun get-kimi-chat-completion (user-prompt)
  "Sends a prompt to the Moonshot AI (Kimi) chat completion API and returns the content of the response.

  Args:
    user-prompt: A string containing the user's message.

  Returns:
    A string with the assistant's reply, or NIL on error."

  (let* ((api-key (uiop:getenv "MOONSHOT_API_KEY"))
         (base-url "https://api.moonshot.ai/v1/chat/completions")
         (payload (alexandria:plist-hash-table
                   `("model" "kimi-k2-0711-preview"
                     "messages" ,(vector
                                  (alexandria:plist-hash-table
                                   '("role" "system"
                                     "content" "You are Kimi, an AI assistant provided by Moonshot AI. You are proficient in English conversations. You provide users with safe, helpful, and accurate answers."))
                                  (alexandria:plist-hash-table
                                   `("role" "user"
                                     "content" ,user-prompt)))
                     "temperature" 0.3)
                   :test 'equal)))

    (unless (and api-key (not (string= api-key "")))
      (error "MOONSHOT_API_KEY environment variable is not set.")
      (return-from get-kimi-chat-completion nil))

    (handler-case
        (let* (;; Encode the payload into a JSON string
               (json-payload (json:encode-json-to-string payload))
               ;; Make the POST request with Dexador
               (response-body (dex:post base-url
                                        :headers `(("Content-Type" . "application/json")
                                                   ("Authorization" . ,(format nil "Bearer ~A" api-key)))
                                        :content json-payload))
               ;; Decode the JSON response from the server. JSON objects become
               ;; alists, and JSON arrays become lists.
               (parsed-response (json:decode-json-from-string response-body))
               ;; Navigate the nested structure to get the message content.
               ;;
               ;; The `choices` key in the JSON corresponds to a JSON array, which
               ;; cl-json decodes as a Lisp list. We use `first` to get the
               ;; first element of that list, instead of `aref` which is for vectors.
               (message-content (cdr (assoc :content
                                            (cdr (assoc :message
                                                        (first (cdr (assoc :choices parsed-response)))))))))
          message-content)
      (dex:http-request-failed (e)
        (format *error-output* "HTTP Request Failed: ~A~%" e)
        (format *error-output* "Response Body: ~A~%" (dex:response-body e))
        nil)
      (error (e)
        (format *error-output* "An unexpected error occurred: ~A~%" e)
        nil))))

(print (get-kimi-chat-completion "What is 1 + 11 + 111?"))

