;;;; run.lisp — Bootstrap and launch the daily-use REPL
;;;;
;;;; Usage:  sbcl --load run.lisp   (from this directory, or anywhere)

(require :asdf)

;; Register the local systems, resolved relative to this file rather than to
;; the current directory, so the script works from any working directory.
(let ((here (or *load-truename* *load-pathname*)))
  (push (make-pathname :directory (pathname-directory here))
        asdf:*central-registry*)
  (dolist (sibling '("litelm" "gemini" "cache_engine"))
    (push (uiop:subpathname here (format nil "../~A/" sibling))
          asdf:*central-registry*)))

;; Load dependencies via Quicklisp
(handler-case
    (ql:quickload '(:daily-use) :silent t)
  (error (c)
    (format t "~%Error loading daily-use: ~A~%" c)
    (format t "~%Make sure you have Quicklisp installed and the following libraries available:~%")
    (format t "  - litelm        (local, ../litelm)~%")
    (format t "  - gemini        (local, ../gemini)~%")
    (format t "  - cache-engine  (local, ../cache_engine, needs sqlite)~%")
    (format t "  - cl-readline   (Quicklisp, requires GNU readline on the system)~%")
    (format t "  - dexador, cl-json, alexandria (Quicklisp, pulled in by the above)~%")
    (format t "~%On macOS, ensure readline is installed:  brew install readline~%")
    (uiop:quit 1)))

;; Verify an API key is available. litelm accepts either variable; the gemini
;; library used for search grounding reads GOOGLE_API_KEY only.
(unless (or (uiop:getenv "GEMINI_API_KEY") (uiop:getenv "GOOGLE_API_KEY"))
  (format t "~%Error: neither GEMINI_API_KEY nor GOOGLE_API_KEY is set.~%")
  (format t "Export one before running:  export GEMINI_API_KEY=your-key-here~%")
  (uiop:quit 1))

(when (null (uiop:getenv "GOOGLE_API_KEY"))
  (format t "~%  [Note: GOOGLE_API_KEY is not set, so the !<query> search-grounding~%")
  (format t "   command will fail. Plain questions will still work.]~%~%"))

;; Launch the REPL
(daily-use:main)
(uiop:quit 0)
