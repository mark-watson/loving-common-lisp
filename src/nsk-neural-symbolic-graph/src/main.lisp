;;;; main.lisp --- Command-line entry point and argument parsing.

(in-package :nsk)

(defparameter *version* "1.0.0")

(defun version ()
  (format nil "NSK ~a" *version*))

(defun quit-lisp (&optional (code 0))
  #+lispworks (funcall (find-symbol "QUIT" :lispworks) :status code)
  #+sbcl (funcall (find-symbol "EXIT" :sb-ext) :code code)
  #-(or lispworks sbcl) (progn code (values)))

(defun command-line-args ()
  "Return the user-supplied command-line arguments as strings."
  #+lispworks
  (let ((sym (find-symbol "*LINE-ARGUMENTS-LIST*" :system)))
    (if (and sym (boundp sym)) (rest (symbol-value sym)) nil))
  #-lispworks
  (let ((uiop-fn (and (find-package :uiop)
                      (find-symbol "COMMAND-LINE-ARGUMENTS" :uiop))))
    (if uiop-fn (funcall uiop-fn) nil)))

(defun flag-present-p (flag args)
  (member flag args :test #'string=))

(defun flag-value (flag args &optional default)
  (let ((pos (position flag args :test #'string=)))
    (if (and pos (< (1+ pos) (length args)))
        (nth (1+ pos) args)
        default)))

(defun print-usage ()
  (format t "~&~a

Usage: nsk [options]

  (no options)     start the interactive REPL
  --serve          start the REST server instead of the REPL
  --port N         server port (default 8800)
  --db PATH        transaction log path (default nsk-graph.log)
  --help           show this message

Wrap the REPL with rlwrap for history and line editing:
  rlwrap nsk
" (version)))

(defun main ()
  "Program entry point. Parse flags, then serve or drop into the REPL."
  (let* ((args (command-line-args))
         (log (or (flag-value "--db" args) (namestring *log-path*)))
         (*log-path* (pathname log)))
    (when (flag-present-p "--help" args)
      (print-usage)
      (quit-lisp 0))
    (handler-case
        (progn
          (setf *graph* (open-store *log-path*))
          (cond
            ((flag-present-p "--serve" args)
             (let ((port (parse-integer (or (flag-value "--port" args) "8800"))))
               (start-server port)
               (format t "~&~a serving on http://localhost:~a  (Ctrl-C to stop)~%"
                       (version) port)
               (loop (sleep 3600))))
            (t (repl))))
      (error (e)
        (format *error-output* "~&fatal: ~a~%" e)))
    (close-store *graph*)
    (quit-lisp 0)))
