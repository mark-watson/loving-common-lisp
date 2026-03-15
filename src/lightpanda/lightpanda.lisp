;;;; lightpanda.lisp — Core interface to the Lightpanda headless browser
;;;;
;;;; Lightpanda is invoked via the `fetch` command which runs the browser
;;;; and returns rendered HTML without needing a server process.
;;;;
;;;; For shell execution this library uses uiop:run-program.

(defpackage #:lightpanda
  (:use #:cl)
  (:export
   ;; Config
   #:*lightpanda-binary*
   ;; Fetch interface
   #:fetch-url
   ;; Helpers
   #:fetch-and-extract-links
   #:demo-fetch))

(in-package #:lightpanda)

;;; -------------------------------------------------------------------------
;;; Configuration
;;; -------------------------------------------------------------------------

(defvar *lightpanda-binary* "lightpanda"
  "Path to the lightpanda binary. Can be an absolute path or a name on PATH.")

;;; -------------------------------------------------------------------------
;;; Internal helpers
;;; -------------------------------------------------------------------------

(defun %run (command)
  "Run a shell command string, return stdout as a string (or nil on error)."
  (handler-case
      (uiop:run-program command
                        :output :string
                        :error-output :string)
    (error (e)
      (format t "Command error: ~a~%Command: ~a~%" e command)
      nil)))

(defun %extract-links (html)
  "Return a list of href strings found in <a> tags within an HTML string."
  (let ((links '())
        (pos    0)
        (marker "href=\""))
    (loop
      (let ((found (search marker html :start2 pos)))
        (unless found (return))
        (let* ((start (+ found (length marker)))
               (end   (position #\" html :start start)))
          (when end
            (push (subseq html start end) links))
          (setf pos (or end (+ found 1))))))
    (nreverse links)))

;;; -------------------------------------------------------------------------
;;; Fetch interface  (lightpanda fetch <url>)
;;; -------------------------------------------------------------------------

(defun fetch-url (url &key (log-level "warn") obey-robots (dump "html"))
  "Fetch URL using `lightpanda fetch`, returning the JS-rendered content string.
DUMP controls what is written to stdout; valid values are:
  \"html\"               - full rendered HTML (default)
  \"markdown\"           - page as Markdown
  \"semantic_tree\"      - semantic tree
  \"semantic_tree_text\" - semantic tree as plain text
No server process is required; lightpanda is invoked directly.

  (lightpanda:fetch-url \"https://markwatson.com/\")
  (lightpanda:fetch-url \"https://markwatson.com/\" :dump \"markdown\")
"
  (let* ((extra (append (when obey-robots '("--obey_robots"))
                        (list "--dump" dump
                              "--log_level" log-level
                              "--log_format" "pretty")))
         (args  (append (list *lightpanda-binary* "fetch")
                        extra
                        (list url)))
         (cmd   (format nil "~{~a~^ ~}" args)))
    (%run cmd)))

;;; -------------------------------------------------------------------------
;;; Helpers
;;; -------------------------------------------------------------------------

(defun fetch-and-extract-links (url)
  "Fetch URL with lightpanda and return a list of href link strings.

  (lightpanda:fetch-and-extract-links \"https://markwatson.com/\")
"
  (let ((html (fetch-url url)))
    (if html
        (%extract-links html)
        (progn
          (format t "Failed to fetch ~a~%" url)
          nil))))

(defun demo-fetch (url)
  "Fetch URL, print a snippet of HTML and the discovered links.

  (lightpanda:demo-fetch \"https://markwatson.com/\")
"
  (format t "~%=== Fetch demo: ~a ===~%" url)
  (let ((html (fetch-url url)))
    (if html
        (progn
          (format t "Received ~a bytes of HTML.~%" (length html))
          (format t "First 500 chars:~%~a~%~%" (subseq html 0 (min 500 (length html))))
          (let ((links (%extract-links html)))
            (format t "Found ~a link(s):~%" (length links))
            (dolist (l links)
              (format t "  ~a~%" l))))
        (format t "No HTML returned.~%"))))
