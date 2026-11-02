;;; Shared helpers for the web-scraping examples in this directory.

(ql:quickload '(:drakma :plump :cl-ppcre) :silent t)

;; DEFPARAMETER, not DEFCONSTANT, so reloading doesn't signal an error.
(defparameter +whitespace+ '(#\Space #\Tab #\Newline #\Return)
  "Characters trimmed from the ends of extracted text.")

(defun trim (string)
  (string-trim +whitespace+ string))

(defun whitespace-only-p (string)
  (every (lambda (char) (member char +whitespace+)) string))

(defun kw (name)
  "Return the keyword symbol for NAME, e.g. \"H1\" => :H1, or NIL."
  (when name (find-symbol (string-upcase name) :keyword)))

(defun text-node-p (node)
  (typep node 'plump:text-node))

(defun element-node-p (node)
  (typep node 'plump:element))

(defun fetch-html (url)
  "Fetch URL as a string, or NIL on failure.
Custom User-Agent (Drakma's default is widely blocked) and UTF-8 (its
Latin-1 fallback garbles pages)."
  (handler-case
      (let ((drakma:*drakma-default-external-format* :utf-8))
        (drakma:http-request url
                             :user-agent "Mozilla/5.0 (compatible; CL-scraping-example)"
                             :connection-timeout 10
                             :redirect 5))
    (error (c)
      (format *error-output* "~&Could not fetch ~A: ~A~%" url c)
      nil)))

(defun node-text (node)
  "Text of NODE's subtree, skipping comments.
PLUMP:TEXT on an element would also collect comment contents."
  (with-output-to-string (s)
    (labels ((walk (node)
               (cond
                 ((text-node-p node) (write-string (plump:text node) s))
                 ((typep node 'plump:nesting-node)
                  (loop for child across (plump:children node)
                        do (walk child))))))
      (walk node))))

;; CL strings have no \n or \t escapes: "\n" is the letter n, and regex
;; "[ \t]" matches a space or the letter t. Use "\\s+"/"[ \\t]+" for regex
;; whitespace, and build real newlines with #\Newline or ~%.

(defun normalize-spaces (string)
  "Collapse every whitespace run, newlines included, to a single space.
Block tags add newlines later, so marker tokens are unnecessary."
  (cl-ppcre:regex-replace-all "\\s+" string " "))

(defun raw-text (node)
  "Text of NODE with spaces/tabs collapsed but newlines kept; comments are
  skipped. For <pre>/<code>, where line structure is content."
  (cl-ppcre:regex-replace-all "[ \\t]+" (node-text node) " "))

(defun escape-markdown (string)
  "Escape characters that Markdown would otherwise interpret as syntax."
  (with-output-to-string (s)
    (loop for char across string
          do (when (find char "*_[]`\\") (write-char #\\ s))
             (write-char char s))))

(defun clean-whitespace (text &key strip-indent)
  "Normalize newlines, drop whitespace-only lines, and collapse runs of
blank lines to one. With STRIP-INDENT, strip leading spaces per line
(plain text only; Markdown needs them for nested lists). Assumes
NORMALIZE-SPACES already ran on text nodes."
  ;; Build newlines from #\Newline; see the escape warning above.
  (let ((two-newlines (format nil "~A~A" (string #\Newline) (string #\Newline))))
    (when strip-indent
      (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+" text "")))
    (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+$" text ""))
    (setf text (cl-ppcre:regex-replace-all "(?m)[ \\t]+$" text ""))
    ;; Match CRLF and lone CR as single units, like Unix newlines.
    (setf text (cl-ppcre:regex-replace-all "(?:\\r\\n|\\r|\\n){3,}" text two-newlines))
    (trim text)))
