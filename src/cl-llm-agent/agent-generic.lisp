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
  (let* ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) text))
         (start (search "```json" trimmed)))
    (if (and start (search "```" trimmed :start2 (1+ start)))
        (let ((json-start (+ start 7))
              (json-end (search "```" trimmed :start2 (1+ start))))
          (subseq trimmed json-start json-end))
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
  (:documentation "Perform an LLM call with AGENT given PROMPT."))

(defun make-agent (agent-type &rest initargs &key context)
  "Construct an agent of AGENT-TYPE with INITARGS and optional CONTEXT."
  (apply #'make-instance agent-type
         (append (when context (list :context context))
                 initargs)))

(defparameter *agent-verbose* nil
  "When true, agent prints debug information.")

(defun get-json-value (obj key)
  "Lookup KEY in OBJ. Handles both plist and alist representations, and keyword/string/symbol keys."
  (cond
    ((null obj) nil)
    ((consp (first obj))
     ;; It's likely an alist
     (let ((cell (or (assoc key obj :test (lambda (k c)
                                            (or (eq k c)
                                                (and (symbolp c) (string-equal (symbol-name k) (symbol-name c)))
                                                (and (stringp c) (string-equal (symbol-name k) c)))))
                     (assoc (symbol-name key) obj :test #'string-equal))))
       (cdr cell)))
    (t
     ;; It's likely a plist
     (loop for (k v) on obj by #'cddr
           when (or (eq k key)
                    (string-equal (symbol-name k) (symbol-name key)))
           return v))))

(defun coerce-to-list (val)
  "Ensure VAL is a list (e.g. converting vectors to lists)."
  (typecase val
    (list val)
    (sequence (coerce val 'list))
    (t (list val))))

(defun resolve-parameters (tool-name params-val)
  "Resolve params-val (which could be a list of positional args, or a map of named args) into a list of positional args for TOOL-NAME."
  (let* ((tool-data (gethash tool-name *tool-registry*))
         (tool-params (and tool-data (getf tool-data :parameters))))
    (cond
      ;; If there are no parameters defined for the tool, return nil
      ((null tool-params) nil)
      ;; If params-val is an alist or plist (named arguments)
      ((and (consp params-val)
            (or (consp (first params-val))
                (keywordp (first params-val))))
       ;; Special case: if the tool has exactly one parameter and the map has exactly one entry,
       ;; just use that entry's value regardless of the key name.
       (if (and (= (length tool-params) 1)
                (or (and (consp (first params-val)) (= (length params-val) 1))
                    (and (keywordp (first params-val)) (= (length params-val) 2))))
           (list (if (consp (first params-val))
                     (cdr (first params-val))
                     (second params-val)))
           ;; General case: map each parameter name to its value
           (loop for param in tool-params
                 collect (get-json-value params-val param))))
      ;; Otherwise, treat it as positional
      (t (coerce-to-list params-val)))))

(defmethod agent-converse ((agent base-agent) user-input)
  "Perform one turn of conversation with AGENT given USER-INPUT."
  (when *agent-verbose*
    (format t "[agent-converse] input: ~A~%" user-input)
    (display-context (agent-context agent) "Context start:"))
  (let* ((tools-info (with-output-to-string (out)
                       (format out "Available Tools:~%")
                       (dolist (tool (list-tools))
                         (format out "  - ~A: ~A~%    Parameters: ~A~%    Example: ~A~%"
                                 (getf tool :name)
                                 (getf tool :description)
                                 (getf tool :parameters)
                                 (getf tool :parameter-example)))))
         (prompt (format nil "~A~%~%User Input: ~A~%~%~
If using tools, you MUST respond ONLY with a JSON object in one of these formats:

Single tool call:
{
  \"action\": \"tool-name\",
  \"parameters\": [\"arg1\", \"arg2\", ...]
}

Multiple chained tool calls (where \"PREV_RESULT\" can be used to pass the result of the previous tool to the next):
{
  \"actions\": [
    { \"action\": \"tool-1\", \"parameters\": [\"arg1\"] },
    { \"action\": \"tool-2\", \"parameters\": [\"PREV_RESULT\"] }
  ]
}

Do not include any markdown formatting, explanation, or extra text. Output only the raw JSON."
                         tools-info user-input))
         (raw (agent-llm-call agent prompt))
         (clean (strip-markdown-json raw))
         (parsed (handler-case
                     (safe-parse-json clean)
                   (error (err)
                     (when *agent-verbose*
                       (format t "[agent-converse] JSON parse error: ~A~%" err))
                     nil))))
    (let ((action-name (or (get-json-value parsed :action)
                           (get-json-value parsed :name)))
          (actions-val (or (get-json-value parsed :actions)
                           (get-json-value parsed :calls))))
      (unless (or action-name actions-val)
        (when *agent-verbose*
          (format t "[agent-converse] returning raw response: ~A~%" clean))
        (return-from agent-converse clean))
      (let ((actions (if actions-val
                         (coerce-to-list actions-val)
                         (list parsed))))
        (when *agent-verbose*
          (format t "[agent-converse] raw: ~A~%clean: ~A~%actions: ~A~%" raw clean actions))
        (loop with prev = nil
              for act in actions
              for name = (or (get-json-value act :action)
                             (get-json-value act :name))
              for params-val = (or (get-json-value act :parameters)
                                   (get-json-value act :arguments))
              for params = (resolve-parameters name params-val)
              do (setf prev (apply #'execute-tool name
                                   (map 'list (lambda (p)
                                                (if (string-equal p "PREV_RESULT") prev p))
                                        params)))
              finally (return (or prev (format nil "~A" raw))))))))
