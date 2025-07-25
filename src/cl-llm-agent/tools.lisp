;;; tools.lisp -- Tool registry and predefined tools for cl-llm-agent
(in-package :cl-llm-agent)

(defvar *tool-registry* (make-hash-table :test #'equal)
  "Registry of registered tools.")

(defun register-tool (name &key description parameters parameter-example function)
  "Register a tool NAME with metadata and FUNCTION."
  (setf (gethash name *tool-registry*)
        (list :name name
              :description description
              :parameters parameters
              :parameter-example parameter-example
              :function function)))

(defun list-tools ()
  "Return a list of registered tools with metadata."
  (loop for data being the hash-value of *tool-registry*
        collect (list :name (getf data :name)
                      :description (getf data :description)
                      :parameters (getf data :parameters)
                      :parameter-example (getf data :parameter-example)
                      :function (getf data :function))))

(defun execute-tool (name &rest args)
  "Execute tool NAME with ARGS, checking parameter counts."
  (let ((data (gethash name *tool-registry*)))
    (unless data (error "Tool ~A not found" name))
    (let ((func   (getf data :function))
          (params (getf data :parameters)))
      (unless (= (length params) (length args))
        (error "Tool ~A expected ~A args but got ~A" name (length params) (length args)))
      (apply func args))))

(defmacro define-tool (name description parameters parameter-example function)
  "Convenience macro to register a tool."
  `(register-tool ,name
                  :description ,description
                  :parameters ',parameters
                  :parameter-example ,parameter-example
                  :function ,function))

;;; Predefined helper functions
(defun helper-read-directory (dir)
  "List files in DIR excluding hidden or backup files."
  (let ((path (truename (or dir "."))))
    (if (probe-file path)
        (remove-if (lambda (n)
                     (or (char= (char n 0) #\#)
                         (char= (char (aref n (1- (length n))) #\~)))
                   (uiop:directory-files path))
        (error "Directory not found: ~A" path))))

(register-tool "tool-read-directory"
               :description "Reads the contents of a directory."
               :parameters '(directory-path)
               :parameter-example "directory-path: string"
               :function #'helper-read-directory)

(defun helper-read-file (file)
  "Return the contents of FILE as a string, or error if missing."
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (with-output-to-string (out)
          (loop for line = (read-line in nil)
                while line do (write-line line out))))
      (error "File not found: ~A" file)))

(register-tool "tool-read-file"
               :description "Reads the contents of a file."
               :parameters '(file-path)
               :parameter-example "file-path: string"
               :function #'helper-read-file)

(register-tool "tool-search-web"
               :description "Search the web with Tavily."
               :parameters '(query)
               :parameter-example "query: string"
               :function (lambda (query) (tavily:websearch query)))

(defun helper-summarize (text)
  "Summarize TEXT using Gemini LLM backend."
  (let ((prompt (format nil "Summarize the following text:~%~A~%" text)))
    (gemini:generate prompt)))

(register-tool "tool-summarize"
               :description "Summarize text using Gemini."
               :parameters '(text)
               :parameter-example "text: string"
               :function #'helper-summarize)