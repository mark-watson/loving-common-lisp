;;; Shared helpers for the web-scraping examples in this directory.
;;; Each example loads this file first:
;;;   (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P"")))

(ql:quickload '(:drakma :plump :cl-ppcre) :silent t)

;; DEFPARAMETER rather than DEFCONSTANT so re-loading these examples in one
;; core does not signal a constant-redefinition error.
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
  "Fetch URL, returning the body as a string, or NIL on failure.
Drakma's default User-Agent is blocked by many sites, so we set our own.
When a server omits the charset, Drakma decodes the body with
*DRAKMA-DEFAULT-EXTERNAL-FORMAT* (Latin-1), which garbles UTF-8 text."
  (handler-case
      (drakma:http-request url
                           :user-agent "Mozilla/5.0 (compatible; CL-scraping-example)"
                           :connection-timeout 10
                           :redirect 5)
    (error (c)
      (format *error-output* "~&Could not fetch ~A: ~A~%" url c)
      nil)))

(defun node-text (node)
  "Text of NODE's subtree, skipping comments.
Do not use PLUMP:TEXT on an element for this: its deep walk also collects
comment contents, so hidden text leaks into the output."
  (with-output-to-string (s)
    (labels ((walk (node)
               (cond
                 ((text-node-p node) (write-string (plump:text node) s))
                 ((typep node 'plump:nesting-node)
                  (loop for child across (plump:children node)
                        do (walk child))))))
      (walk node))))

;; NOTE ON REGEX STRINGS: Common Lisp string tokens have no Python-style
;; \n or \t escapes. Inside "...", a backslash merely makes the next
;; character literal, so "\n" is the single character #\n (code 110), not
;; a newline, and "[ \t]" is a class matching space and the letter t.
;; Regex escapes therefore need double backslashes ("\\s+", "[ \\t]+"),
;; and real newline characters must be built from #\Newline (string
;; #\Newline) or ~% (format nil "~A~%") instead of "\n".

(defun normalize-spaces (string)
  "Collapse every run of whitespace, newlines included, into a single space.
Block tags supply the newlines during assembly, so marker tokens (such as
the old __H1__ hack) are not needed to protect them from CLEAN-WHITESPACE."
  (cl-ppcre:regex-replace-all "\\s+" string " "))

(defun raw-text (node)
  "Text of NODE's subtree that collapses spaces/tabs but keeps newlines,
  and skips comments. For <pre>/<code>, where line structure is content."
  (cl-ppcre:regex-replace-all "[ \\t]+" (node-text node) " "))

(defun escape-markdown (string)
  "Escape characters that Markdown would otherwise interpret as syntax."
  (with-output-to-string (s)
    (loop for char across string
          do (when (find char "*_[]`\\") (write-char #\\ s))
             (write-char char s))))

(defun clean-whitespace (text &key strip-indent)
  "Normalize newlines, drop whitespace-only lines, and collapse runs of
blank lines to one. With STRIP-INDENT also remove leading spaces per line
(safe for plain text; Markdown needs them for nested-list indentation).
Text nodes are assumed to be normalized already by NORMALIZE-SPACES, so no
space-collapse pass is needed here."
  ;; Build replacement strings from #\Newline; see the escape warning above.
  (let ((two-newlines (format nil "~A~A" (string #\Newline) (string #\Newline))))
    (when strip-indent
      (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+" text "")))
    (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+$" text ""))
    (setf text (cl-ppcre:regex-replace-all "(?m)[ \\t]+$" text ""))
    ;; Match CRLF and lone CR as single units so Windows-sourced line
    ;; endings collapse the same way Unix ones do.
    (setf text (cl-ppcre:regex-replace-all "(?:\\r\\n|\\r|\\n){3,}" text two-newlines))
    (trim text)))
