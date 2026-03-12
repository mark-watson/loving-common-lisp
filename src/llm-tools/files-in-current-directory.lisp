;;;; files-in-current-directory.lisp
;;;;
;;;; A command-line tool that prints "important" files in the current directory,
;;;; one per line. "Important" means source files, Makefiles, PDFs, text files,
;;;; etc. — not compiled artifacts, hidden dirs, or binary blobs.
;;;;
;;;; Usage: ./bin/files-in-current-directory [directory]
;;;;   If no directory argument is given, uses the current working directory.

(require :asdf)  ; ensures UIOP is available (bundled with SBCL)

(defpackage #:files-in-current-directory
  (:use #:cl)
  (:export #:main))

(in-package #:files-in-current-directory)

;;; Extensions considered "important" (lowercase, no dot).
(defparameter *important-extensions*
  '("lisp" "lsp" "cl" "asd"
    "scm" "ss" "rkt"
    "py" "pyw"
    "js" "mjs" "ts" "tsx" "jsx"
    "rb" "go" "rs"
    "c" "h" "cc" "cpp" "hpp"
    "java" "kt" "scala"
    "sh" "bash" "zsh" "fish"
    "yaml" "yml" "toml" "ini" "cfg"
    "json" "xml" "html" "htm" "css"
    "md" "markdown" "rst" "txt"
    "pdf" "csv" "tsv" "sql"
    "r" "jl" "tex" "bib"))

;;; Exact basenames always included.
(defparameter *important-basenames*
  '("Makefile" "makefile" "GNUmakefile"
    "Dockerfile" "docker-compose.yml" "docker-compose.yaml"
    "Gemfile" "Rakefile" "Justfile"
    ".gitignore" ".gitattributes"
    "LICENSE" "LICENCE" "COPYING" "AUTHORS"
    "CHANGELOG" "CHANGES" "TODO" "NOTES" "INSTALL"))

(defun file-extension (filename)
  "Return the lowercased extension of FILENAME, or NIL if none."
  (let ((dot (position #\. filename :from-end t)))
    (when (and dot (< dot (1- (length filename))))
      (string-downcase (subseq filename (1+ dot))))))

(defun important-p (pathname)
  "Return T if PATHNAME is an important file."
  (let ((name (file-namestring pathname)))
    (or (member name *important-basenames* :test #'string=)
        (let ((ext (file-extension name)))
          (and ext (member ext *important-extensions* :test #'string=))))))

(defun collect-important-files (directory)
  "Recursively collect important files under DIRECTORY, sorted."
  (let ((results '()))
    (labels ((walk (dir)
               (dolist (entry (uiop:directory* (merge-pathnames uiop:*wild-file-for-directory* dir)))
                 (cond
                   ;; Sub-directory — recurse (skip hidden dirs like .git)
                   ((uiop:directory-pathname-p entry)
                    (let ((dname (car (last (pathname-directory entry)))))
                      (unless (and (stringp dname) (char= (char dname 0) #\.))
                        (walk entry))))
                   ;; Regular file — include if important
                   (t
                    (when (important-p entry)
                      (push (uiop:native-namestring entry) results)))))))
      (walk (uiop:ensure-directory-pathname directory)))
    (sort results #'string<)))

(defun main ()
  "Entry point: print important files, one per line."
  (let* ((args (uiop:command-line-arguments))
         (target-dir (if args (first args) (uiop:getcwd))))
    (handler-case
        (dolist (f (collect-important-files target-dir))
          (format t "~A~%" f))
      (error (e)
        (format *error-output* "Error: ~A~%" e)
        (sb-ext:exit :code 1))))
  (sb-ext:exit :code 0))
