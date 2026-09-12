;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Provider registry and model routing: "provider/model-name" -> endpoint.

(in-package #:litelm)

(defstruct provider
  name          ; keyword, e.g. :openai
  base-url      ; OpenAI-compatible base URL, e.g. "https://api.openai.com/v1"
  env-keys      ; list of environment variables to try for the API key
  (requires-key t))

(defvar *providers* (make-hash-table :test 'eq))

(defun define-provider (name base-url &key env-keys (requires-key t))
  "Register a provider. NAME is a keyword, BASE-URL an OpenAI-compatible
base URL, ENV-KEYS a list of environment variable names tried in order."
  (setf (gethash name *providers*)
        (make-provider :name name
                       :base-url base-url
                       :env-keys (if (listp env-keys) env-keys (list env-keys))
                       :requires-key requires-key)))

(define-provider :openai "https://api.openai.com/v1"
  :env-keys '("OPENAI_API_KEY" "OPENAI_KEY"))

(define-provider :gemini "https://generativelanguage.googleapis.com/v1beta/openai"
  :env-keys '("GEMINI_API_KEY" "GOOGLE_API_KEY"))

(define-provider :fireworks-ai "https://api.fireworks.ai/inference/v1"
  :env-keys '("FIREWORKS_API_KEY"))

(define-provider :deepseek "https://api.deepseek.com/v1"
  :env-keys '("DEEPSEEK_API_KEY"))

(define-provider :ollama "http://localhost:11434/v1"
  :env-keys nil
  :requires-key nil)

(defun find-provider (name)
  (or (gethash name *providers*)
      (error 'litelm-error
             :format-control "Unknown provider ~S. Known providers: ~S"
             :format-arguments (list name (loop for k being the hash-keys of *providers*
                                                collect k)))))

(defun parse-model (model &key provider)
  "Split a \"provider/model-name\" string into (values provider model-name).
PROVIDER keyword argument overrides the prefix."
  (cond
    (provider
     (values (find-provider provider) model))
    ((and (stringp model) (find #\/ model))
     (let ((slash (position #\/ model)))
       (values (find-provider (intern (string-upcase (subseq model 0 slash)) :keyword))
               (subseq model (1+ slash)))))
    (t (error 'litelm-error
              :format-control "Model ~S must be of the form \"provider/model-name\""
              :format-arguments (list model)))))

(defun provider-api-key (provider explicit-key)
  (or explicit-key
      (loop for var in (provider-env-keys provider)
            for value = (uiop:getenv var)
            when (and value (plusp (length value))) return value)
      (if (provider-requires-key provider)
          (error 'litelm-error
                 :format-control "No API key for provider ~S. Pass :api-key or set one of ~S"
                 :format-arguments (list (provider-name provider)
                                         (provider-env-keys provider)))
          nil)))

(defun provider-headers (api-key)
  (append '(("Content-Type" . "application/json"))
          (when api-key
            (list (cons "Authorization" (concatenate 'string "Bearer " api-key))))))

(defun provider-url (provider path api-base)
  (concatenate 'string (or api-base (provider-base-url provider)) path))
