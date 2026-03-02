;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License

(defpackage #:simple-tools
  (:use #:cl)
  (:export #:*tools*
           #:tool
	   #:define-tool
           #:make-tool
           #:tool-name
           #:tool-description
           #:tool-parameters
           #:tool-fn
           #:define-tool
           #:call-tool
           #:render-tool
           #:map-args-to-parameters))

(in-package #:simple-tools)

(defstruct tool
  "A tool callable by LLM completions."
  name
  description
  parameters
  fn)

(defvar *tools* (make-hash-table :test 'equalp))

(defmacro define-tool (name args description &body body)
  "Define a tool callable by LLM completions.
ARGS is a list of (param-name type description) triples."
  (let ((name-str (if (symbolp name) (string-downcase (symbol-name name)) name))
        (arg-names (mapcar (lambda (arg) (intern (string-upcase (first arg)))) args)))
    `(setf (gethash ,name-str *tools*)
           (make-tool
            :name ,name-str
            :description ,description
            :parameters ',args
            :fn (lambda ,arg-names ,@body)))))

(defun call-tool (tool-name &rest args)
  "Call a tool by name with the given arguments."
  (let ((tool (gethash tool-name *tools*)))
    (if tool
        (apply (tool-fn tool) args)
        (error "Unknown tool: ~A" tool-name))))

(defun render-tool (tool)
  "Render a tool as a JSON schema alist for LLM API requests."
  `((:type . "function")
    (:function . ,(remove nil
                    `((:name . ,(tool-name tool))
                      (:description . ,(tool-description tool))
                      ,(when (tool-parameters tool)
                         `(:parameters . ((:type . "object")
                                          (:properties
					   .
					   ,(loop for p in (tool-parameters tool)
                                                  collect (list
							   (first p)
                                                           (cons :type (second p))
                                                           (cons :description (third p)))))
                                          (:required
					   .
					   ,(loop for p in (tool-parameters tool)
                                                  collect (first p)))))))))))

(defun map-args-to-parameters (tool args)
  "Map an alist of JSON arguments to positional values in the tool's declared parameter order."
  (loop for (param-name param-type param-desc) in (tool-parameters tool)
        collect (rest (assoc (intern (string-upcase param-name) :keyword) args))))
