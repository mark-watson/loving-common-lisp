;;; agent-gemini.lisp -- Gemini-based agent implementation
(in-package :cl-llm-agent)

;; Define a specialized agent using the Gemini LLM back-end
(defclass gemini-agent (base-agent) ()
  (:documentation "Agent using Gemini LLM back-end."))

(defmethod agent-llm-call ((agent gemini-agent) prompt)
  "Perform an LLM call for GEMINI-AGENT using gemini:generate."
  (declare (ignore agent))
  (gemini:generate prompt))