;; Copyright Mark Watson 2001-2013. All Rights Reserved.
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.

(defclass HTMLstream ()
  ((out :accessor out))
  (:documentation "Provide HTML generation services"))

(defmethod set-header ((ho HTMLstream) title)
  (with-accessors
      ((out out))
      ho
    (setf out (make-string-output-stream))
    (princ "<HTML><head><title>" out)
    (princ title out)
    (princ "</title></head><BODY>" out)
    (terpri out)))

(defmethod add-H1 ((ho HTMLstream) some-text)
  (with-accessors
   ((out out))
   ho
   (princ "<H1>" out)
   (princ some-text out)
   (princ "</H1>" out)
   (terpri out)))

(defmethod add-H2 ((ho HTMLstream) some-text)
  (with-accessors
   ((out out))
   ho
   (princ "<H2>" out)
   (princ some-text out)
   (princ "</H2>" out)
   (terpri out)))

(defmethod add-H3 ((ho HTMLstream) some-text)
  (with-accessors
   ((out out))
   ho
   (princ "<H3>" out)
   (princ some-text out)
   (princ "</H3>" out)
   (terpri out)))

(defmethod add-H4 ((ho HTMLstream) some-text)
  (with-accessors
   ((out out))
   ho
   (princ "<H4>" out)
   (princ some-text out)
   (princ "</H4>" out)
   (terpri out)))

(defmethod add-element ((ho HTMLstream) element)
  (with-accessors
      ((out out))
      ho
    (princ element out)
    (terpri out)))
  
(defmethod add-table ((ho HTMLstream) table-data)
  (with-accessors
      ((out out))
      ho
    (princ "<TABLE BORDER=\"1\" WIDTH=\"100%\">" out)
    (dolist (d table-data)
      (terpri out)
      (princ "  <TR>" out)
      (terpri out)
      (dolist (w d)
        (princ "    <TD>" out)
	(let ((str (princ-to-string w)))
	  (setq str (string-left-trim '(#\() str))
	  (setq str (string-right-trim '(#\)) str))
	  (princ str out))
        (princ "</TD>" out)
        (terpri out))
      (princ "  </TR>" out)
      (terpri out))
    (princ "</TABLE>" out)
    (terpri out)))
               

(defmethod get-html-string ((ho HTMLstream))
  (with-accessors
      ((out out))
      ho
  (princ "</BODY></HTML>" out)
  (terpri out)
  (get-output-stream-string out)))

(defun test (&aux x)
 (setq x (make-instance 'HTMLstream))
 (set-header x "test page")
 (add-element x "test text - this could be any element")
 (add-table
  x
  '(("<b>Key phrase</b>" "<b>Ranking value</b>")
    ("this is a test" 3.3)))
 (get-html-string x))
