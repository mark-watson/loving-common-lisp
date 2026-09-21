;;; tools.lisp -- File-system tools for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

;;; ---- Helper functions executed locally ----

(defun tool-list-directory (dir)
  "List files and subdirectories in DIR.
   Excludes hidden and backup entries.
   Returns a newline-separated string of names
   relative to DIR."
  ;; TRUENAME canonicalises the directory.  Without it a relative
  ;; argument such as \".\" stays relative, and ENOUGH-NAMESTRING cannot
  ;; strip an absolute entry name against a relative default -- so the
  ;; model would receive full absolute paths instead of relative ones.
  ;; It also turns a misspelled directory into an error rather than an
  ;; empty listing.
  (let* ((resolved (truename
                    (uiop:ensure-directory-pathname (or dir "."))))
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

;;; ---- litelm tool definitions ----
;;;
;;; Tools use litelm's Lisp format:
;;;   (name description ((param type desc) ...))

(defun %make-tool-declarations ()
  "Build the list of litelm tool definitions
   for file-system tools."
  '((list_directory
     "List files and subdirectories in a directory.
      Returns one entry per line."
     ((path "string"
       "Absolute or relative directory path")))
    (read_file
     "Read the full contents of a text file and
      return it as a string."
     ((path "string"
       "Absolute or relative file path")))
    (write_file
     "Create or overwrite a file with the given
      content.  Parent directories are created
      automatically."
     ((path "string"
       "Absolute or relative file path")
      (content "string"
       "The full text content to write")))))

;;; ---- Dispatch a tool-call plist ----

(defun dispatch-tool-call (fc)
  "Execute the tool described by tool-call plist
   FC (:ID :NAME :ARGUMENTS) as returned by
   litelm:response-tool-calls.
   Returns a string result."
  (let* ((name (getf fc :name))
         (args (getf fc :arguments))
         (get-arg (lambda (key)
                    (let ((pair (assoc key args
                                       :test #'string-equal)))
                      ;; Report a missing argument by name.  Letting NIL
                      ;; reach the tool would surface a raw SBCL type error
                      ;; ("NIL is not of type ...") instead of something the
                      ;; model can act on.
                      (unless (and pair (cdr pair))
                        (error "Missing required argument ~S for tool ~A"
                               key name))
                      (cdr pair)))))
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
