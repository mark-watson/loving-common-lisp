;; Copyright Mark Watson 2001-2013. All Rights Reserved.  http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.

(in-package #:myutils)

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


(defun words-from-string (str)
  (let ((ss (tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))

(defun tokenize-string-keep-uri (string)
  (tokenize-string string 
                   :delimiters '(#\Space #\Return #\Linefeed #\Newline #\, #\;  #\( #\)) ;;  #\?) ;; will fail with URIs with '?' or "\""
                   :discard '(#\Space #\Return #\Linefeed #\Newline #\,)))

(defun tokenize-string (string 
                        &key 
                          (delimiters '(#\Space #\Return #\Linefeed #\Newline #\. #\, #\; #\: #\! #\" #\'
                                        #\? #\/ #\( #\) #\- #\< #\>))
                          (discard '(#\Space #\Return #\Linefeed #\Newline #\, #\" #\' #\- #\< #\>))
                          (test (lambda (c) (find c delimiters)))
                          (start 0) (end (length string)) (omit-delimiters nil))
  (flet ((get-token (start)
           (if (< start end)
               (let* ((delimiterp (funcall test (char string start)))
                      (end-of-token (funcall (if delimiterp #'position-if-not #'position-if)
                                             test string :start start)))
                 (values (subseq string start end-of-token) end-of-token delimiterp))
               (values nil nil nil))))
    (let ((tokens nil)
          token delimiterp)
      (loop (multiple-value-setq (token start delimiterp) (get-token start))
         (unless (and delimiterp omit-delimiters)
           (let ((tok (string-trim discard token)))
             ;;(print (list "tok:" tok))
             (if (not (find tok discard))
                 (if (> (length tok) 0)
                     (push tok tokens)))))
         (unless start (return-from tokenize-string (nreverse tokens)))))))

(defun file-to-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; really baroque: improve this:
(defun node-name-from-uri (uri)
  (replace-all
   (replace-all
    (replace-all
     (replace-all
      (replace-all
       (replace-all
	(replace-all
	 (replace-all
	  (replace-all
	   (replace-all
	    (replace-all
	     (replace-all
	      (replace-all uri "https://" "")
	      "http://" "")
	     "/" "_")
	    "." "_")
	   " " "_")
	  "-" "_")
	 "(" "")
	")" "")
       "<" "")
      ">" "")
     "?" "")
    "=" "_")
   "'" ""))
