;;; context.lisp -- Context management for cl-llm-agent
(in-package :cl-llm-agent)

(defclass context ()
  ((data :initform (make-hash-table :test #'equal)
         :accessor context-data))
  (:documentation "Context class for storing key-value pairs."))

(defun make-context ()
  "Create a new agent context."
  (make-instance 'context))

(defgeneric context-set (ctx key value)
  "Set KEY to VALUE in CTX.")
(defmethod context-set ((ctx context) key value)
  (setf (gethash key (context-data ctx)) value))

(defgeneric context-get (ctx key)
  "Retrieve the value for KEY from CTX.")
(defmethod context-get ((ctx context) key)
  (gethash key (context-data ctx)))

(defgeneric context-remove (ctx key)
  "Remove KEY from CTX.")
(defmethod context-remove ((ctx context) key)
  (remhash key (context-data ctx)))

(defun display-context (ctx &optional (message "Context contents:"))
  "Pretty print the contents of CTX."  
  (format t "~A~%" message)
  (let ((table (context-data ctx)))
    (if (hash-table-p table)
        (maphash (lambda (k v)
                   (format t "  ~A: ~A~%" k v))
                 table)
        (format t "  Invalid context object~%"))))