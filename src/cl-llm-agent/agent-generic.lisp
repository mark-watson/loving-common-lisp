;;; agent-generic.lisp -- Core agent definitions for cl-llm-agent
(in-package :cl-llm-agent)

;; JSON parsing utilities
(defun parse-json (json-string)
  "Parse JSON-STRING into Lisp data; returns nil on failure."
  (ignore-errors
    (json:set-decoder-simple-list-semantics)
    (cl-json:decode-json-from-string json-string)))

(defun safe-parse-json (json-string)
  "Parse JSON-STRING, or signal an error if parsing fails."
  (or (parse-json json-string)
      (error "Failed to parse JSON: ~A" json-string)))

(defun strip-markdown-json (text)
  "Remove markdown fences around JSON in TEXT."  
  (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) text)))
    (if (and (search "```json" trimmed)
             (search "```" trimmed :start2 (1+ (search "```json" trimmed))))
        (subseq trimmed
                (search "```json" trimmed)
                (search "```" trimmed :start2 (1+ (search "```json" trimmed))))
        trimmed)))

;; Macro to define new agent classes
(defmacro define-agent (name &body body)
  "Define a new agent CLASS NAME with optional (:bases ...) and class BODY forms."
  (let ((bases nil)
        (forms body))
    (when (and body (consp (first body)) (eq (first (first body)) :bases))
      (setf bases (second (first body))
            forms (rest body)))
    `(defclass ,name ,bases
       ,@forms
       (:documentation ,(format nil "Agent class ~A" name)))))

;; Base agent class
(defclass base-agent ()
  ((name    :initarg :name    :accessor agent-name :initform nil)
   (context :initarg :context :accessor agent-context)
   (tools   :initform (make-hash-table :test #'equal) :accessor agent-tools))
  (:documentation "Base class for LLM agents."))

(defmethod initialize-instance :after ((agent base-agent) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp agent 'context)
    (setf (slot-value agent 'context) (make-context))))

;; LLM back-end generic
(defgeneric agent-llm-call (agent prompt)
  "Perform an LLM call with AGENT given PROMPT.")

(defun make-agent (agent-type &rest initargs &key context)
  "Construct an agent of AGENT-TYPE with INITARGS and optional CONTEXT."
  (apply #'make-instance agent-type
         (append (when context (list :context context))
                 initargs)))

(defparameter *agent-verbose* nil
  "When true, agent prints debug information.")

(defmethod agent-converse ((agent base-agent) user-input)
  "Perform one turn of conversation with AGENT given USER-INPUT."
  (when *agent-verbose*
    (format t "[agent-converse] input: ~A~%" user-input)
    (display-context (agent-context agent) "Context start:"))
  (let* ((tools-info (with-output-to-string (out)
                       (format out "tools:~%")
                       (dolist (t (list-tools))
                         (format out "  ~A: ~A~%"
                                 (getf t :name)
                                 (getf t :description)))))
         (prompt (format nil "~A~%User Input: ~A~%~%If using tools, respond in JSON." tools-info user-input))
         (raw (agent-llm-call agent prompt))
         (clean (strip-markdown-json raw))
         (parsed (safe-parse-json clean))
         (actions (if (getf parsed :actions)
                      (getf parsed :actions)
                      (list parsed))))
    (when *agent-verbose*
      (format t "[agent-converse] raw: ~A~%clean: ~A~%actions: ~A~%" raw clean actions))
    (loop with prev = nil
          for action across actions
          for name   = (getf action :action)
          for params = (getf action :parameters)
          do (setf prev (apply #'execute-tool name
                               (map 'list (lambda (p)
                                            (if (string-equal p "PREV_RESULT") prev p))
                                    params)))
          finally (return (or prev (format nil "~A" raw)))))