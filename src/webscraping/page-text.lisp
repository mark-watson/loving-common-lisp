(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))

(defun text-boilerplate-p (tag)
  "Elements dropped for plain-text extraction. The Markdown example keeps
nav/header/footer because their link lists carry meaning; here we want only
the article body. Named differently from MD-BOILERPLATE-P so both examples
can be loaded in one core without clobbering each other."
  (member tag '("script" "style" "head" "nav" "header" "footer" "iframe" "noscript")
          :test #'string-equal))

(defun get-element-spacing (tag text)
  "Wrap TEXT in newlines appropriate for TAG. CLEAN-WHITESPACE collapses
over-long runs afterward, so each block only needs enough newlines to
separate it from its neighbors."
  (case (kw tag)
    ((:h1 :h2 :h3 :h4 :h5 :h6 :p :blockquote :ul :ol :pre)
     (format nil "~%~A~%~%" text))
    ((:li :tr)
     (format nil "~A~%" text))
    (:br
     (format nil "~%"))
    ((:div :article :section :aside :main)
     (if (and (plusp (length text))
              (char= #\Newline (char text (1- (length text)))))
         text
         (format nil "~A~%" text)))
    (t text)))

(defun children-text (node)
  (with-output-to-string (s)
    (loop for child across (plump:children node)
          do (write-string (get-clean-text child) s))))

(defun get-clean-text (node)
  "Recursively collect text from NODE, skipping boilerplate tags and
inserting line breaks for block tags. <pre>/<code> keep their internal
newlines; all other text is whitespace-normalized at the node level."
  (cond
    ((text-node-p node)
     (normalize-spaces (plump:text node)))
    ((element-node-p node)
     (let ((tag (plump:tag-name node)))
       (if (text-boilerplate-p tag)
           ""
           (let ((text (if (member (kw tag) '(:pre :code))
                           (raw-text node)
                           (children-text node))))
             (get-element-spacing tag text)))))
    ((typep node 'plump:nesting-node)
     (children-text node))
    (t "")))

(defun fetch-and-print-text (&optional (url "https://markwatson.com"))
  "Fetch URL and print cleaned-up text content."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (format t "~A~%"
              (clean-whitespace (get-clean-text (plump:parse html))
                                :strip-indent t)))))

(fetch-and-print-text)
