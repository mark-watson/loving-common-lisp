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
    *gemini-endpoint*
  "https://generativelanguage.googleapis.com/v1beta/models/")
(defvar *gemini-model* "gemini-3-flash-preview")

(defun get-google-api-key ()
  (uiop:getenv "GOOGLE_API_KEY"))

(defun generate (prompt &key tools (model-id *gemini-model*))
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    
    ;; Handle Tool Stubs
    (when tools
      (let ((function-declarations
             (loop for tool-symbol in tools
                   collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                             (if tool
                                 (cdr (assoc :function (simple-tools:render-tool tool)))
                                 (error "Undefined tool function: ~A" tool-symbol))))))
        (setf (gethash "tools" payload)
              (list (let ((tool-decl (make-hash-table :test 'equal)))
                      (setf (gethash "functionDeclarations" tool-decl) function-declarations)
                      tool-decl)))))
    
    (let* ((api-url (concatenate 'string *gemini-endpoint* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (llm:escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url (get-google-api-key) escaped-json))
           ;;(ignore 1 (format t "~%~%$$ curl command:~%~A~%" curl-cmd))
           (response-string (llm:run-curl-command curl-cmd))
           ;;(ignore2 (format t "~%~%$$ response:~%~A~%" response-string))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts)))
      ;; Check for functionCall
      (let ((function-call (cdr (assoc :FUNCTION-CALL first-part))))
        (if function-call
            (let* ((name (cdr (assoc :NAME function-call)))
                   (args (or (cdr (assoc :ARGS function-call))
                             (cdr (assoc :ARGUMENTS function-call))))
                   (tool (gethash name simple-tools:*tools*))
                   (mapped-args (simple-tools:map-args-to-parameters tool args)))
              (apply (simple-tools:tool-fn tool) mapped-args))
            (let ((text-pair (assoc :TEXT first-part)))
              (cdr text-pair)))))))

(defun count-tokens (prompt &optional (model-id *gemini-model*))
  (let* ((api-url (concatenate 'string *gemini-endpoint* model-id ":countTokens"))
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
    (let* ((api-url (concatenate 'string *gemini-endpoint* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (llm:escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url (get-google-api-key) escaped-json))
           (response-string (llm:run-curl-command curl-cmd))
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

(defun generate-with-search-and-citations (prompt &optional (model-id *gemini-model*))
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
    (let* ((api-url (concatenate 'string *gemini-endpoint* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (llm:escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url (get-google-api-key) escaped-json))
           (response-string (llm:run-curl-command curl-cmd))
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
           (text (cdr text-pair))
           (metadata-pair (assoc :GROUNDING-METADATA candidate))
           (metadata (cdr metadata-pair))
           (chunks-pair (assoc :GROUNDING-CHUNKS metadata))
           (chunks (cdr chunks-pair))
           (citations (loop for chunk in chunks
                            for web-data-pair = (assoc :WEB chunk)
                            for web-data = (cdr web-data-pair)
                            when web-data
                            collect (cons (cdr (assoc :TITLE web-data))
                                          (cdr (assoc :URI web-data))))))
      (values text citations))))
