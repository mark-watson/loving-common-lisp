;; Copyright Mark Watson 2019. All Rights Reserved.   http://markwatson.com
;; License: Either Apache 2 or AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)

(in-package :entities_dbpedia)

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(print a1)
(print (length a1))
(defvar *current-directory* (subseq a1 3 (- (length a1) 22)))

(print "current directory:")
(print *current-directory*)

;;                    INITIALIZE DATA - this may take a minute or two

(print "Startng to load data....")

(defvar *broadcast* (make-hash-table :test #'equal :size 4000))
(defvar *cities* (make-hash-table :test #'equal :size 4000))
(defvar *companys* (make-hash-table :test #'equal :size 400))
(defvar *countries* (make-hash-table :test #'equal :size 400))
(defvar *music_groups* (make-hash-table :test #'equal :size 400))
(defvar *people* (make-hash-table :test #'equal :size 400))
(defvar *political_parties* (make-hash-table :test #'equal :size 400))
(defvar *trade_unions* (make-hash-table :test #'equal :size 400))
(defvar *universities* (make-hash-table :test #'equal :size 400))

(defvar *EntityNames* '())

(defun get-entity-names ()
  *EntityNames*)

(defun tokenize-tabbed-line (line) ;; by stack overflow account beoliver
  (loop 
     for start = 0 then (+ space 1)
     for space = (position #\Tab line :start start)
     for token = (subseq line start space)
     collect token until (not space)))

(defun load-line-file (hash-table file-name)
  (setf *EntityNames* (cons (myutils:replace-all file-name "NamesDbPedia.txt" "") *EntityNames*))
  (let (line)
    (with-open-file (in (concatenate 'string *current-directory* "/data/" file-name))
      (dotimes (i 50000)
	(setq line (read-line in nil nil))
	(if (null line) (return)
	    (let ((tokens (split-sequence:SPLIT-SEQUENCE #\Tab line)))
	      (if (eq 2 (length tokens))
		  (let ((entity-name (car tokens))
			(uri (cadr tokens)))
		    (setf (gethash entity-name hash-table) uri)))))))))

(load-line-file *broadcast* "BroadcastNetworkNamesDbPedia.txt")
(load-line-file *cities* "CityNamesDbPedia.txt")
(load-line-file *companys* "CompanyNamesDbPedia.txt")
(load-line-file *countries* "CountryNamesDbPedia.txt")
(load-line-file *music_groups* "MusicGroupNamesDbPedia.txt")
(load-line-file *people* "PeopleNamesDbPedia.txt")
(load-line-file *political_parties* "PoliticalPartyNamesDbPedia.txt")
(load-line-file *trade_unions* "TradeUnionNamesDbPedia.txt")
(load-line-file *universities* "UniversityNamesDbPedia.txt")

(print "....done loading data.")

;;;   done loading data

;; Conversion of KnowledgeBooks.com Java FastTag to Common Lisp


(defun words-from-string (str)
  (let ((ss (tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))

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

(defun find-entities-in-string (entity-hash str)
    (let* ((words (words-from-string str)))
      (find-entities-word-list entity-hash words)))
;;
;; utility for detecting entities in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-entities-in-word-list (entity-hash words)
  (let* ((len (length words))
         word
         (ret '()))
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 4 word entities:
      (if (< i (- len 3))
	  (let* ((4-words (concatenate 'string (aref words i) " " (aref words (+ i 1)) " "
				       (aref words (+ i 2)) " " (aref words (+ i 3))))
		 (a-uri (gethash 4-words entity-hash)))
	    (if a-uri
		(setq ret (cons (list 4-words a-uri) ret)))))
      ;; 3 word entities:
      (if (< i (- len 2))
	  (let* ((3-words (concatenate 'string (aref words i) " " (aref words (+ i 1))
				       " " (aref words (+ i 2))))
		 (a-uri (gethash 3-words entity-hash)))
	    (if a-uri
		(setq ret (cons (list 3-words a-uri) ret)))))
      ;; 2 word entities:
      (if (< i (- len 1))
	  (let* ((2-words (concatenate 'string (aref words i) " " (aref words (+ i 1))))
		 (a-uri (gethash 2-words entity-hash)))
	    (if a-uri
		(setq ret (cons (list 2-words a-uri) ret)))))
      ;; 1 word entity:
      (let ((a-uri (gethash word entity-hash)))
	(if a-uri
	    (setq ret (cons (list word a-uri) ret)))))
    (reverse ret)))


(defun find-entities-in-text (str)
  (let* ((words (words-from-string str))
	 (bc (find-entities-in-word-list *broadcast* words))
	 (ci (find-entities-in-word-list *cities* words))
	 (co (find-entities-in-word-list *companys* words))
	 (cn (find-entities-in-word-list *countries* words))
	 (mg (find-entities-in-word-list *music_groups* words))
	 (pe (find-entities-in-word-list *people* words))
	 (pp (find-entities-in-word-list *political_parties* words))
	 (tu (find-entities-in-word-list *trade_unions* words))
	 (un (find-entities-in-word-list *universities* words))
	 (h (make-hash-table :test #'equal :size 30)))
    (if bc (setf (gethash "broadcast" h) bc))
    (if ci (setf (gethash "cities" h) ci))
    (if co (setf (gethash "companies" h) co))
    (if cn (setf (gethash "countries" h) cn))
    (if mg (setf (gethash "music_groups" h) mg))
    (if pe (setf (gethash "people" h) pe))

    (if pp (setf (gethash "political_parties" h) pp))
    (if tu (setf (gethash "trade_union" h) tu))
    (if un (setf (gethash "universities" h) un))
    h)) ;; return hash with defined entities
 

(defun entity-iterator (a-function a-hash-table)
  (maphash a-function a-hash-table))

;; just for debug:


(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun test1 ()
  (let ((h (find-entities-in-text "Bill Clinton and George Bush went to Mexico and England and watched Univision. They enjoyed Dakbayan sa Dabaw and shoped at Best Buy and listened to Al Stewart. They agree on República de Nicaragua and support Sweden Democrats and Leicestershire Miners Association and both sent their kids to Darul Uloom Deoband.")))
    (entity-iterator #'print-hash-entry h)))

(defun pp-entities (str)
  (maphash #'print-hash-entry (find-entities-in-text str)))

#|

(find-entities *people* "Bill Clinton and George Bush")

(find-entities-in-text "Bill Clinton and George Bush went to Mexico and England and watched Univision. They enjoyed Dakbayan sa Dabaw and shoped at Best Buy and listened to Al Stewart. They agree on República de Nicaragua and support Sweden Democrats and Leicestershire Miners Association and both sent their kids to Darul Uloom Deoband.")

(pp-entities "Bill Clinton and George Bush went to Mexico and England")

|#
