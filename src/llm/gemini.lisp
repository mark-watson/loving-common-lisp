;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:gemini
  (:use #:cl #:llm)
  (:export #:gemini-llm
           #:generate
           #:count-tokens
           #:chat
           #:generate-with-search
           #:generate-with-search-and-citations))

(in-package #:gemini)

(defvar
    *interactions-api-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")
(defvar *gemini-model* "gemini-3-flash-preview")

(defun get-google-api-key ()
  (uiop:getenv "GOOGLE_API_KEY"))

;;; ---- Interactions API helpers ----

(defun %interactions-curl-cmd (payload)
  "Build a curl command for the Interactions API with the new steps schema."
  (let* ((data (cl-json:encode-json-to-string payload))
         (escaped-json (llm:escape-json data)))
    (format nil
      "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -H \"Api-Revision: 2026-05-20\" -d \"~A\""
      *interactions-api-url* (get-google-api-key) escaped-json)))

(defun %extract-text-from-steps (decoded-response)
  "Extract the text from the last model_output step in an Interactions API response."
  (let* ((steps (cdr (assoc :STEPS decoded-response))))
    (loop for step in (reverse steps)
          when (string-equal (cdr (assoc :TYPE step)) "model_output")
          return (let* ((content (cdr (assoc :CONTENT step)))
                        (first-content (first content)))
                   (cdr (assoc :TEXT first-content))))))

(defun generate (prompt &key tools (model-id *gemini-model*))
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt)
    
    ;; Handle Tool Stubs
    (when tools
      (let ((tool-list
             (loop for tool-symbol in tools
                   collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                             (if tool
                                 (let ((rendered (cdr (assoc :function (simple-tools:render-tool tool))))
                                       (fn-tool (make-hash-table :test 'equal)))
                                   (setf (gethash "type" fn-tool) "function"
                                         (gethash "name" fn-tool) (cdr (assoc :name rendered))
                                         (gethash "description" fn-tool) (cdr (assoc :description rendered))
                                         (gethash "parameters" fn-tool) (cdr (assoc :parameters rendered)))
                                   fn-tool)
                                 (error "Undefined tool function: ~A" tool-symbol))))))
        (setf (gethash "tools" payload) tool-list)))
    
    (let* ((curl-cmd (%interactions-curl-cmd payload))
           (response-string (llm:run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps (cdr (assoc :STEPS decoded-response))))
      ;; Check for function_call steps
      (let ((fc-step (find-if (lambda (step)
                                (string-equal (cdr (assoc :TYPE step)) "function_call"))
                              steps)))
        (if fc-step
            (let* ((name (cdr (assoc :NAME fc-step)))
                   (args (or (cdr (assoc :ARGUMENTS fc-step))
                             (cdr (assoc :ARGS fc-step))))
                   (tool (gethash name simple-tools:*tools*))
                   (mapped-args (simple-tools:map-args-to-parameters tool args)))
              (apply (simple-tools:tool-fn tool) mapped-args))
            (%extract-text-from-steps decoded-response))))))

(defun count-tokens (prompt &optional (model-id *gemini-model*))
  "NOTE: countTokens still uses the models endpoint (no Interactions API equivalent)."
  (let* ((api-url (concatenate 'string
                               "https://generativelanguage.googleapis.com/v1beta/models/"
                               model-id ":countTokens"))
         (payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (escaped-json (llm:escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url (get-google-api-key) escaped-json))
           (response-string (llm:run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S" decoded-response)))))

(defvar *chat-history* '())

(defun chat ()
  (let ((*chat-history* ""))
   (loop
     (princ "Enter a prompt: ")
     (finish-output)
     (let ((user-prompt (read-line)))
       (princ user-prompt)
       (finish-output)
       (let ((gemini-response (generate
                (concatenate 'string *chat-history* "\nUser: " user-prompt))))
         (princ gemini-response)
         (finish-output)
         (setf *chat-history*
               (concatenate 'string "User: " user-prompt "\n" "Gemini: " gemini-response
                                   "\n" *chat-history* "\n\n")))))))

(defun generate-with-search (prompt &optional (model-id *gemini-model*))
  "Generates text with Google Search grounding via the Interactions API."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (let* ((curl-cmd (%interactions-curl-cmd payload))
           (response-string (llm:run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string)))
      (%extract-text-from-steps decoded-response))))

(defun generate-with-search-and-citations (prompt &optional (model-id *gemini-model*))
  "Generates text with Google Search grounding and returns citations.
   Returns two values: text and a list of (title . url) citation pairs."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (let* ((curl-cmd (%interactions-curl-cmd payload))
           (response-string (llm:run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps (cdr (assoc :STEPS decoded-response)))
           ;; Extract text from last model_output step
           (text (loop for step in (reverse steps)
                       when (string-equal (cdr (assoc :TYPE step)) "model_output")
                       return (let* ((content (cdr (assoc :CONTENT step)))
                                     (first-content (first content)))
                                (cdr (assoc :TEXT first-content)))))
           ;; Extract citations from url_citation annotations
           (citations
            (loop for step in steps
                  when (string-equal (cdr (assoc :TYPE step)) "model_output")
                  append (loop for content-item in (cdr (assoc :CONTENT step))
                               append (loop for annotation in (cdr (assoc :ANNOTATIONS content-item))
                                            when (string-equal (cdr (assoc :TYPE annotation)) "url_citation")
                                            collect (cons (cdr (assoc :TITLE annotation))
                                                          (cdr (assoc :URL annotation))))))))
      (values text citations))))
