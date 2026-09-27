(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))
(ql:quickload :clss :silent t)

(defun fetch-and-print-headers (&optional (url "https://markwatson.com"))
  "Fetch URL and print the text of h1-h6 tags in document order.
One comma-separated selector beats six round trips through the DOM, and
NODE-TEXT keeps HTML comments out of the printed headers."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (let ((nodes (clss:select "h1,h2,h3,h4,h5,h6" (plump:parse html))))
        (if (zerop (length nodes))
            (format t "No h1-h6 headers found.~%")
            (loop for node across nodes
                  for text = (trim (normalize-spaces (node-text node)))
                  unless (string= text "")
                    do (format t "  [~A] ~A~%" (plump:tag-name node) text)))))))

(fetch-and-print-headers)
