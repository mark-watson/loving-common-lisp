;; Copyright Mark Watson 2001-2022. All Rights Reserved.  http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.

(in-package #:dbpedia)


;; utility from http://cl-cookbook.sourceforge.net/strings.html#manip:
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurrences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defstruct dbpedia-data uri label description)

(defun dbpedia-lookup (search-string)
  (let* ((s-str (replace-all search-string " " "+"))
         (s-uri
          (concatenate
           'string
           "https://lookup.dbpedia.org/api/search?query="
           s-str))
         (response-body nil)
         (response-status nil)
         (response-headers nil)
         (xml nil)
         ret)
    (multiple-value-setq
        (response-body response-status response-headers)
      (drakma:http-request
       s-uri
       :method :get
       :accept "application/xml"))
    (unless (= response-status 200)
      (error "DBpedia lookup failed with status ~A" response-status))
    (let ((xml-str (typecase response-body
                     (string response-body)
                     (vector (babel:octets-to-string response-body))
                     (t (error "Invalid or empty response body from DBpedia")))))
      (setf xml (s-xml:parse-xml-string xml-str)))
    (dolist (r (cdr xml))
      (let ((uri (cadr (find "uri" (cdr r)
                             :key (lambda (el) (and (consp el) (symbol-name (first el))))
                             :test #'string-equal)))
            (label (cadr (find "Label" (cdr r)
                               :key (lambda (el) (and (consp el) (symbol-name (first el))))
                               :test #'string-equal)))
            (desc (cadr (find "Description" (cdr r)
                              :key (lambda (el) (and (consp el) (symbol-name (first el))))
                              :test #'string-equal))))
        (push
         (make-dbpedia-data
          :uri uri
          :label label
          :description (and desc (string-trim '(#\Space #\NewLine #\Tab) desc)))
         ret)))
    (reverse ret)))

;; (dbpedia:dbpedia-lookup "berlin")
