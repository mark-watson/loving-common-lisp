;;; tools.lisp -- File-system tools for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

;;; ---- Helper functions executed locally ----

(defun tool-list-directory (dir)
  "List files and subdirectories in DIR.
   Excludes hidden and backup entries.
   Returns a newline-separated string of pathnames."
  (let* ((resolved (uiop:ensure-directory-pathname
                    (or dir ".")))
         (entries
          (append (uiop:directory-files resolved)
                  (uiop:subdirectories resolved))))
    (if entries
        (with-output-to-string (out)
          (dolist (e entries)
            (let ((name (enough-namestring e resolved)))
              (unless (or (uiop:string-prefix-p "." name)
                          (uiop:string-suffix-p "~" name)
                          (uiop:string-prefix-p "#" name))
                (format out "~A~%" name)))))
        (format nil "(empty directory: ~A)" resolved))))

(defun tool-read-file (path)
  "Return the contents of PATH as a string.
   Signals an error when the file does not exist."
  (let ((truepath (probe-file path)))
    (unless truepath
      (error "File not found: ~A" path))
    (uiop:read-file-string truepath)))

(defun tool-write-file (path content)
  "Write CONTENT to PATH, creating parent directories
   as needed.  Returns a confirmation message."
  (let ((pathname (pathname path)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (write-string content out))
    (format nil "Wrote ~D characters to ~A"
            (length content) path)))

;;; ---- Gemini function declarations ----

(defun %make-tool-declarations ()
  "Build the list of Gemini function declarations
   for file-system tools."
  (list
   (gemini:make-function-declaration
    "list_directory"
    "List files and subdirectories in a directory.
     Returns one entry per line."
    '(("path" "STRING"
       "Absolute or relative directory path"))
    '("path"))

   (gemini:make-function-declaration
    "read_file"
    "Read the full contents of a text file and
     return it as a string."
    '(("path" "STRING"
       "Absolute or relative file path"))
    '("path"))

   (gemini:make-function-declaration
    "write_file"
    "Create or overwrite a file with the given
     content.  Parent directories are created
     automatically."
    '(("path" "STRING"
       "Absolute or relative file path")
      ("content" "STRING"
       "The full text content to write"))
    '("path" "content"))))

;;; ---- Dispatch a function-call plist ----

(defun dispatch-tool-call (fc)
  "Execute the tool described by function-call
   plist FC (:NAME :ID :ARGS).
   Returns a string result."
  (let* ((name (getf fc :name))
         (args (getf fc :args))
         (get-arg (lambda (key)
                    (cdr (assoc key args
                                :test #'string-equal)))))
    (handler-case
        (cond
          ((string-equal name "list_directory")
           (tool-list-directory
            (funcall get-arg "path")))
          ((string-equal name "read_file")
           (tool-read-file
            (funcall get-arg "path")))
          ((string-equal name "write_file")
           (tool-write-file
            (funcall get-arg "path")
            (funcall get-arg "content")))
          (t (format nil "Unknown tool: ~A" name)))
      (error (e)
        (format nil "Tool error (~A): ~A" name e)))))
