;;; agent-gemini.lisp -- Gemini-based agent implementation
(in-package :cl-llm-agent)

;; Default model used when none is provided explicitly.
(defparameter *default-gemini-model-id* "gemini-pro"
  "Fallback Gemini model identifier used by the agent.")

(defun normalize-gemini-model-id (model-id)
  "Ensure MODEL-ID uses the format expected by gemini:generate."
  (let ((id (or model-id *default-gemini-model-id*)))
    (if (uiop:string-prefix-p "models/" id)
        (subseq id (length "models/"))
        id)))

(defun default-gemini-model-id ()
  "Return the Gemini model identifier to use for new agents."
  (normalize-gemini-model-id (uiop:getenv "GEMINI_MODEL")))

;; Define a specialized agent using the Gemini LLM back-end
(defclass gemini-agent (base-agent)
  ((model-id :initarg :model-id
             :accessor gemini-agent-model-id
             :initform (default-gemini-model-id)
             :documentation "Gemini model identifier to use for LLM calls."))
  (:documentation "Agent using Gemini LLM back-end."))

(defmethod agent-llm-call ((agent gemini-agent) prompt)
  "Perform an LLM call for GEMINI-AGENT using gemini:generate."
  (gemini:generate (normalize-gemini-model-id (gemini-agent-model-id agent))
                   prompt))
