;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Main entry points: COMPLETION and EMBEDDING, plus the error hierarchy.

(in-package #:litelm)

;;; ---- conditions (mirrors litelm's exception hierarchy) ----

(define-condition litelm-error (simple-error) ())

(define-condition api-error (litelm-error)
  ((status :initarg :status :reader api-error-status)
   (body :initarg :body :reader api-error-body))
  (:report (lambda (c stream)
             (format stream "LLM API error ~A: ~A"
                     (api-error-status c) (api-error-body c)))))

(define-condition authentication-error (api-error) ())
(define-condition rate-limit-error (api-error) ())
(define-condition not-found-error (api-error) ())
(define-condition context-window-exceeded-error (api-error) ())

(defun %map-http-error (status body)
  "Map an HTTP status code to the corresponding litelm condition."
  (let ((condition
          (cond ((member status '(401 403)) 'authentication-error)
                ((= status 429) 'rate-limit-error)
                ((= status 404) 'not-found-error)
                ((and (= status 400)
                      (search "context" (or body "") :test #'char-equal))
                 'context-window-exceeded-error)
                (t 'api-error))))
    (error condition :status status :body body)))

;;; ---- response ----

(defstruct response
  "The result of a COMPLETION call."
  content        ; string or nil when the model only made tool calls
  tool-calls     ; list of plists (:id id :name name :arguments keyword-alist)
  finish-reason
  model
  usage          ; plist (:prompt-tokens n :completion-tokens n :total-tokens n)
  raw)           ; the full decoded JSON response alist

;;; ---- HTTP ----

(defun %post (url headers payload)
  "POST PAYLOAD (alist) as JSON to URL. Returns the decoded JSON alist."
  (let ((body (json-encode payload)))
    (handler-case
        (json-decode (dex:post url :headers headers :content body :read-timeout 120 :connect-timeout 60))
      (dex:http-request-failed (e)
        (%map-http-error (dex:response-status e) (dex:response-body e))))))


;;; ---- streaming ----

(defun %handle-sse-stream (stream on-chunk)
  "Read an OpenAI-style SSE stream, calling ON-CHUNK with each content delta.
Returns the accumulated content string."
  (let ((acc (make-string-output-stream)))
    (unwind-protect
         (loop for line = (read-line stream nil nil)
               while line
               when (and (>= (length line) 5) (string= line "data:" :end1 5))
                 do (let ((data (string-trim '(#\Space) (subseq line 5))))
                      (if (string= data "[DONE]")
                          (return)
                          (let* ((json (json-decode data))
                                 (delta (aget (first (aget json "choices")) "delta"))
                                 (text (and delta (aget delta "content"))))
                            (when text
                              (write-string text acc)
                              (funcall on-chunk text))))))
      (close stream))
    (get-output-stream-string acc)))

;;; ---- main entry points ----

(defun completion (model &key messages tools (stream nil) (tool-choice nil)
                              temperature max-tokens top-p
                              api-key api-base extra-headers)
  "Send a chat completion to MODEL, a \"provider/model-name\" string.

MESSAGES is a string or a list of (role content &rest options) messages.
TOOLS is a list of (name description ((param type desc &key required enum) ...)).
STREAM is nil, t (print deltas), or a function called with each content delta.
TOOL-CHOICE is nil, :auto, :none, or :required.

Returns a RESPONSE struct; use RESPONSE-CONTENT and RESPONSE-TOOL-CALLS.
Tool calls are returned, not executed — execution is the caller's job."
  (multiple-value-bind (provider model-name) (parse-model model)
    (let* ((key (provider-api-key provider api-key))
           (headers (append (provider-headers key) extra-headers))
           (url (provider-url provider "/chat/completions" api-base))
           (payload
             (append
              `(("model" . ,model-name)
                ("messages" . ,(translate-messages messages)))
              (when tools
                `(("tools" . ,(translate-tools tools))))
              (when tool-choice
                `(("tool_choice" . ,(%name-string tool-choice))))
              (when temperature `(("temperature" . ,temperature)))
              (when max-tokens `(("max_tokens" . ,max-tokens)))
              (when top-p `(("top_p" . ,top-p)))
              (when stream `(("stream" . t))))))
      (if stream
          (let ((on-chunk (if (functionp stream)
                              stream
                              (lambda (text) (write-string text) (finish-output)))))
            (handler-case
                (let ((http-stream
                        (dex:post url :headers headers
                                      :content (json-encode payload)
                                      :want-stream t
                                      :read-timeout 120
                                      :connect-timeout 60)))
                  (make-response :content (%handle-sse-stream http-stream on-chunk)
                                 :model model-name))
              (dex:http-request-failed (e)
                (%map-http-error (dex:response-status e) (dex:response-body e)))))
          (let* ((json (%post url headers payload))
                 (choice (first (aget json "choices")))
                 (message (and choice (aget choice "message"))))
            (make-response
             :content (and message (aget message "content"))
             :tool-calls (and message
                              (mapcar #'parse-response-tool-call
                                      (aget message "tool_calls")))
             :finish-reason (and choice (aget choice "finish_reason"))
             :model (or (aget json "model") model-name)
             :usage (parse-usage (aget json "usage"))
             :raw json))))))

(defun embedding (model input &key api-key api-base)
  "Compute embeddings for INPUT (a string or list of strings) using MODEL,
a \"provider/model-name\" string. Returns a list of float lists."
  (multiple-value-bind (provider model-name) (parse-model model)
    (let* ((key (provider-api-key provider api-key))
           (headers (provider-headers key))
           (url (provider-url provider "/embeddings" api-base))
           (payload `(("model" . ,model-name)
                      ("input" . ,(if (stringp input) (list input) input))))
           (json (%post url headers payload)))
      (loop for item in (aget json "data")
            collect (aget item "embedding")))))
