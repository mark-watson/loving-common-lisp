(ql:quickload '(:drakma :plump :clss))

(defun fetch-and-print-headers (url)
  "Fetches the URL and prints text from H1 to H6 tags using drakma, plump, and clss."
  (format t "Fetching ~A...~%" url)
  (let* ((html-content (drakma:http-request url))
         (parsed-html (plump:parse html-content)))
    (dolist (tag '("h1" "h2" "h3" "h4" "h5" "h6"))
      (format t "~A sections:~%" tag)
      (let ((nodes (clss:select tag parsed-html)))
        (loop for node across nodes do
          (let ((text (plump:text node)))
            (when text
              (format t "  - ~A~%" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))))))))

(fetch-and-print-headers "https://markwatson.com")
