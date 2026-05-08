;;;; run.lisp — Bootstrap and launch the daily-use REPL
;;;;
;;;; Usage:  sbcl --load run.lisp

(require :asdf)

;; Register local systems
(push (truename "./") asdf:*central-registry*)
(push (truename "../gemini/") asdf:*central-registry*)
(push (truename "../cache_engine/") asdf:*central-registry*)

;; Load dependencies via Quicklisp
(handler-case
    (ql:quickload '(:daily-use) :silent t)
  (error (c)
    (format t "~%Error loading daily-use: ~A~%" c)
    (format t "~%Make sure you have Quicklisp installed and the following libraries available:~%")
    (format t "  - cl-json~%")
    (format t "  - cl-readline  (requires GNU readline on the system)~%")
    (format t "  - sqlite~%")
    (format t "~%On macOS, ensure readline is installed:  brew install readline~%")
    (uiop:quit 1)))

;; Verify GOOGLE_API_KEY is set
(unless (uiop:getenv "GOOGLE_API_KEY")
  (format t "~%Error: GOOGLE_API_KEY environment variable is not set.~%")
  (format t "Export it before running:  export GOOGLE_API_KEY=your-key-here~%")
  (uiop:quit 1))

;; Launch the REPL
(daily-use:main)
(uiop:quit 0)
