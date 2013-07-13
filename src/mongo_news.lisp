;; Copyright Mark Watson 2001-2013. All Rights Reserved.
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.

(ql:quickload "cl-mongo")

(cl-mongo:db.use "news")

(defun add-article (uri title text)
  (let ((doc (cl-mongo:make-document)))
	(cl-mongo:add-element "uri" uri doc)
	(cl-mongo:add-element "title" title doc)
	(cl-mongo:add-element "text" text doc)
	(cl-mongo:db.insert "article" doc)))
	
;; example: (add-article "http://test.com" "article title 1" "article text 1")

(defun print-articles ()
  (cl-mongo:pp (cl-mongo:iter (cl-mongo:db.find "article" :all))))

;; for each document, use the cl-mongo:get-element on each element we want to save:
(defun article-results->lisp-data (mdata)
  (let ((ret '()))
    (print (list "size of result=" (length mdata)))
    (dolist (a mdata)
      (print a)
      (setf 
       ret
       (cons
         (list
          (cl-mongo:get-element "uri" a)
          (cl-mongo:get-element "title" a)
          (cl-mongo:get-element "text" a))
         ret)))
     ret))

(defun get-articles ()
  (article-results->lisp-data
    (cadr (cl-mongo:db.find "article" :all))))

;; try: (cl-mongo:pp (cl-mongo:iter (cl-mongo:db.find "article" (cl-mongo:kv "uri" "http://test.com"))))

;; try: (cl-mongo:pp (cl-mongo:iter (cl-mongo:db.find "article" (cl-mongo:kv "title" (cl-mongo:kv "$regex" "article")) :limit 10)))

(defun search-articles-title (str) ;; find documents where substring 'str' is in the title
  (article-results->lisp-data
    (cadr (cl-mongo:iter (cl-mongo:db.find "article" (cl-mongo:kv "title" (cl-mongo:kv "$regex" str)) :limit 10)))))

(defun search-articles-text (str) ;; find documents where substring 'str' is in the text element
  (article-results->lisp-data
    (cadr (cl-mongo:db.find "article" (cl-mongo:kv "text" (cl-mongo:kv "$regex" str)) :limit 10))))
