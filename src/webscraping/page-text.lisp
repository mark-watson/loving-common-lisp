(ql:quickload '(:drakma :plump :cl-ppcre))

(defun text-node-p (node)
  (typep node 'plump:text-node))

(defun element-node-p (node)
  (typep node 'plump:element))

(defun ignore-tag-p (tag)
  (member tag '("script" "style" "head" "nav" "header" "footer" "iframe" "noscript")
          :test #'string-equal))

(defun get-element-spacing (tag text)
  "Returns the text wrapped in appropriate layout formatting or markers based on the tag."
  (cond
    ((string-equal tag "h1")
     (format nil "~%__H1__~%~A~%__H1__~%" text))
    ((string-equal tag "h2")
     (format nil "~%__H2__~%~A~%__H2__~%" text))
    ((member tag '("h3" "h4" "h5" "h6") :test #'string-equal)
     (format nil "~%~A~%~%" text))
    ((member tag '("p" "blockquote" "pre" "ul" "ol") :test #'string-equal)
     (format nil "~%~A~%~%" text))
    ((string-equal tag "br")
     (format nil "~A~%" text))
    ((member tag '("li" "tr") :test #'string-equal)
     (format nil "~A~%" text))
    ((member tag '("div" "article" "section" "aside" "main") :test #'string-equal)
     (if (and (> (length text) 0) (not (char= (char text (1- (length text))) #\Newline)))
         (format nil "~A~%" text)
         text))
    (t text)))

(defun get-clean-text (node)
  "Recursively collects text from NODE, skipping non-content tags and inserting linebreaks/markers for tags."
  (cond
    ((text-node-p node)
     (plump:text node))
    ((element-node-p node)
     (let ((tag (plump:tag-name node)))
       (if (ignore-tag-p tag)
           ""
           (let ((text (with-output-to-string (s)
                         (loop for child across (plump:children node)
                               do (write-string (get-clean-text child) s)))))
             (get-element-spacing tag text)))))
    ((typep node 'plump:nesting-node)
     (with-output-to-string (s)
       (loop for child across (plump:children node)
             do (write-string (get-clean-text child) s))))
    (t "")))

(defun clean-whitespace (text)
  "Cleans up excessive spaces and newlines in the text, preserving spacing for H1/H2."
  (let* ((n (format nil "~%"))
         ;; 1. Clean up lines that contain only whitespace
         (text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+$" text ""))
         ;; 2. Collapse double spaces
         (text (cl-ppcre:regex-replace-all "[ \\t]+" text " "))
         ;; 3. Collapse multiple consecutive newlines to at most 2 newlines (1 blank line)
         (text (cl-ppcre:regex-replace-all (format nil "~A{3,}" n) text (format nil "~A~A" n n)))
         ;; 4. Replace __H1__ markers with 4 newlines (3 blank lines)
         (text (cl-ppcre:regex-replace-all (format nil "~A*__H1__~A*" n n) text (format nil "~A~A~A~A" n n n n)))
         ;; 5. Replace __H2__ markers with 3 newlines (2 blank lines)
         (text (cl-ppcre:regex-replace-all (format nil "~A*__H2__~A*" n n) text (format nil "~A~A~A" n n n)))
         ;; 6. Trim leading/trailing whitespace of the whole text
         (text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    text))

(defun fetch-and-print-text (url)
  "Fetches the URL and prints cleaned up text content."
  (format t "Fetching ~A...~%" url)
  (let* ((html-content (drakma:http-request url))
         (parsed-html (plump:parse html-content))
         (raw-text (get-clean-text parsed-html))
         (cleaned-text (clean-whitespace raw-text)))
    (format t "~A~%" cleaned-text)))

(fetch-and-print-text "https://markwatson.com")
