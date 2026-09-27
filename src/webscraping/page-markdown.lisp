(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))

(defun md-boilerplate-p (tag)
  "Elements removed before Markdown conversion. Unlike page-text.lisp,
nav/header/footer are kept: their link lists are often the point of the
conversion. Named apart from TEXT-BOILERPLATE-P so both examples can be
loaded in one core without clobbering each other."
  (member (kw tag) '(:script :style :head :noscript :iframe) :test #'eq))

(defun md-heading (level inner)
  (format nil "~%~A ~A~%~%"
          (make-string level :initial-element #\#) (trim inner)))

(defun whitespace-child-p (child)
  "Between <li> tags, text nodes are source indentation, not content."
  (and (text-node-p child) (whitespace-only-p (plump:text child))))

(defun html-to-markdown (node &optional (depth 0) (ordered nil) (index 1))
  "Recursively convert the plump HTML node NODE into Markdown.
DEPTH counts enclosing lists (nested items indent two spaces per level),
ORDERED is true inside an <ol>, and INDEX is an <li>'s position within its
list. Text nodes are Markdown-escaped and whitespace-normalized up front;
block tags contribute the newlines, so CLEAN-WHITESPACE needs no marker
tokens to protect spacing."
  (cond
    ((text-node-p node)
     (normalize-spaces (escape-markdown (plump:text node))))
    ((element-node-p node)
     (let ((tag (kw (plump:tag-name node))))
       (cond
         ((md-boilerplate-p tag) "")
         ((member tag '(:ul :ol) :test #'eq)
          (with-output-to-string (s)
            (let ((item 0))
              (loop for child across (plump:children node)
                    unless (whitespace-child-p child)
                      do (incf item)
                         (write-string (html-to-markdown child (1+ depth)
                                                         (eq tag :ol) item)
                                       s)))))
         (t
          (let ((inner (if (member tag '(:pre :code) :test #'eq)
                           (raw-text node)
                           (with-output-to-string (s)
                             (loop for child across (plump:children node)
                                   do (write-string (html-to-markdown child depth
                                                                      ordered index)
                                                    s))))))
            (case tag
              (:h1 (md-heading 1 inner))
              (:h2 (md-heading 2 inner))
              (:h3 (md-heading 3 inner))
              (:h4 (md-heading 4 inner))
              (:h5 (md-heading 5 inner))
              (:h6 (md-heading 6 inner))
              ((:p :blockquote)
               (format nil "~%~A~%~%" (trim inner)))
              (:br
               (format nil "~%~A" inner))
              ((:strong :b)
               (format nil "**~A**" inner))
              ((:em :i)
               (format nil "*~A*" inner))
              (:code
               (format nil "`~A`" (trim inner)))
              (:pre
               (format nil "~%~%```~%~A~%```~%~%" (trim inner)))
              (:a
               (let ((href (plump:attribute node "href")))
                 (if (and href (plusp (length (trim inner))))
                     (format nil "[~A](~A)" (trim inner) href)
                     inner)))
              (:img
               (let ((src (plump:attribute node "src"))
                     (alt (escape-markdown (or (plump:attribute node "alt") ""))))
                 (if src
                     (format nil "![~A](~A)" alt src)
                     "")))
              (:li
               (let ((indent (make-string (* 2 (max 0 (1- depth)))
                                          :initial-element #\Space))
                     (marker (if ordered (format nil "~D. " index) "* ")))
                 (format nil "~A~A~A~%" indent marker (trim inner))))
              ((:div :article :section :aside :main :tr :table :td :th)
               (format nil "~%~A~%~%" inner))
              (t inner)))))))
    ((typep node 'plump:nesting-node)
     (with-output-to-string (s)
       (loop for child across (plump:children node)
             do (write-string (html-to-markdown child depth) s))))
    (t "")))

(defun fetch-and-print-markdown (&optional (url "https://markwatson.com"))
  "Fetch URL and print the content converted to Markdown."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (format t "~A~%"
              (clean-whitespace (html-to-markdown (plump:parse html)))))))

(fetch-and-print-markdown)
