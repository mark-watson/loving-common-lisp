;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Translation between the Common Lisp friendly message/tool formats and the
;;; OpenAI-compatible wire format (nested alists ready for JSON encoding).
;;;
;;; Message format:
;;;   ((:system "You are terse.")
;;;    (:user "What's the weather in Paris?")
;;;    (:assistant nil :tool-calls ((:id "call_1" :name get_weather
;;;                                   :arguments "{\"location\":\"Paris\"}")))
;;;    (:tool "sunny, 22C" :tool-call-id "call_1"))
;;;
;;; Tool definition format:
;;;   ((get_weather "Get the current weather for a location"
;;;      ((location "string" "City name")              ; required by default
;;;       (units "string" "celsius or fahrenheit" :required nil
;;;              :enum ("celsius" "fahrenheit"))))
;;;    ...)

(in-package #:litelm)

(defun %name-string (x)
  "Symbols/strings -> downcased name string."
  (etypecase x
    (string x)
    (symbol (string-downcase (symbol-name x)))))

;;; ---- messages ----

(defun translate-tool-call (tc)
  `(("id" . ,(getf tc :id))
    ("type" . "function")
    ("function" . (("name" . ,(%name-string (getf tc :name)))
                   ("arguments" . ,(let ((args (getf tc :arguments)))
                                     (if (stringp args) args (json-encode args))))))))

(defun translate-message (msg)
  "Translate one Lisp-format message to a wire-format alist."
  (destructuring-bind (role content &rest options) msg
    (append
     (list (cons "role" (%name-string role)))
     (when content (list (cons "content" content)))
     (loop for (k v) on options by #'cddr
           collect (cons (json-key k)
                         (if (eq k :tool-calls)
                             (mapcar #'translate-tool-call v)
                             v))))))

(defun translate-messages (messages)
  "MESSAGES is a string (single user message) or a list of Lisp-format messages."
  (if (stringp messages)
      (list (translate-message (list :user messages)))
      (mapcar #'translate-message messages)))

;;; ---- tools ----

(defun translate-parameter (param)
  "PARAM is (name type description &key (required t) enum)."
  (destructuring-bind (name type description &key enum &allow-other-keys) param
    (cons (%name-string name)
          (append (list (cons "type" (%name-string type))
                        (cons "description" description))
                  (when enum (list (cons "enum" enum)))))))

(defun translate-tool (tool)
  "Translate a Lisp-format tool definition to a wire-format alist."
  (destructuring-bind (name description parameters) tool
    `(("type" . "function")
      ("function" .
       (("name" . ,(%name-string name))
        ("description" . ,description)
        ("parameters" .
         (("type" . "object")
          ("properties" . ,(mapcar #'translate-parameter parameters))
          ("required" . ,(loop for p in parameters
                               when (getf (cdddr p) :required t)
                                 collect (%name-string (first p)))))))))))

(defun translate-tools (tools)
  (mapcar #'translate-tool tools))

;;; ---- responses ----

(defun %keywordize-arguments (alist)
  "Convert a decoded JSON alist into an alist with keyword keys."
  (loop for (k . v) in alist
        collect (cons (intern (string-upcase k) :keyword) v)))

(defun parse-response-tool-call (tc)
  "Parse a wire-format tool call into a plist
(:id id :name name :arguments keyword-alist)."
  (let* ((fn (aget tc "function"))
         (args-json (aget fn "arguments")))
    (list :id (aget tc "id")
          :name (aget fn "name")
          :arguments (and args-json (%keywordize-arguments (json-decode args-json))))))

(defun parse-usage (usage)
  (when usage
    (list :prompt-tokens (aget usage "prompt_tokens")
          :completion-tokens (aget usage "completion_tokens")
          :total-tokens (aget usage "total_tokens"))))
