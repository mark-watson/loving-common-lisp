;; Copyright Mark Watson 2001-2013. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; Common Lisp stemming code in this file was written by Steven M. Haflich based on the work of Martin Porter.


(in-package :kbnlp)

;; data structures

(defstruct text
  url
  title
  summary
  category-tags
  key-words
  key-phrases
  human-names
  place-names
  company-names
  text
  tags)



;;                    INITIALIZE DATA - this may take a minute or two

(print "Starting to load data....")

(defvar *male* "male")
(defvar *female* "female")
(defvar *male-or-female* "male-or-female")
(defvar *first-name-hash* (make-hash-table :test #'equal :size 4000))
(defvar *last-name-hash* (make-hash-table :test #'equal :size 4000))
(defvar *company-name-hash* (make-hash-table :test #'equal :size 400))
(defvar *product-name-hash* (make-hash-table :test #'equal :size 400))

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(defvar *current-directory* (subseq a1 3 (- (length a1) 11)))

(load (concatenate 'string *current-directory* "linguistic_data/cat-data-tables"))

(let (line)
  (with-open-file
      (in (concatenate 'string *current-directory* "linguistic_data/names/names.male"))
    (dotimes (i 50000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *first-name-hash*) *male*)))
  (with-open-file
      (in (concatenate 'string *current-directory* "linguistic_data/names/names.female"))
    (dotimes (i 50000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (if (null (gethash line *first-name-hash*))
          (setf (gethash line *first-name-hash*) *female*)
          (setf (gethash line *first-name-hash*) *male-or-female*)))))

(let (line)
  (with-open-file
      (in (concatenate 'string *current-directory* "linguistic_data/names/names.last"))
    (dotimes (i 5000000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *last-name-hash*) *male*))))

(if (not (boundp 'place-hash))
    (progn
      (load (concatenate 'string *current-directory* "linguistic_data/names/PlaceData.ldat"))))

(if (not (boundp 'stop-word-hash))
    (progn
      (load (concatenate 'string *current-directory* "linguistic_data/names/StopWords.ldat"))))

(let (line)
  (with-open-file
      (in (concatenate 'string *current-directory* "linguistic_data/names/names.companies"))
    (dotimes (i 500000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *company-name-hash*) t))))

(let (line)
  (with-open-file
      (in (concatenate 'string *current-directory* "linguistic_data/names/names.products"))
    (dotimes (i 500000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *product-name-hash*) t))))

(print "....done loading data.")

;;;   done loading data


(defun words-from-string (str)
  (let ((ss (myutils:tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))

;; e.g.:
;;
;; (defun test5 (x) (not (equal x #\ )))
;; (tokens "the dog ran" #'test5 0)




;;  utility for string replacement:

;; http://cl-cookbook.sourceforge.net/strings.html#manip:
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




;;          DETECT NAMES IN TEXT:


(defvar *name-prefix-list*
  '("Mr" "Mrs" "Ms" "Gen" "General" "Maj" "Major" "Doctor" "Vice" "President" "Lt"
    "Premier" "Senator" "Congressman" "Prince" "King" "Representative" "Sen" "St" "Dr"))


(defun not-in-list-find-names-helper (a-list start end &aux (rval t))
  (dolist (x a-list)
    (let ((i1 (car x))
          (i2 (cadr x)))
      (if (or
           (and
            (>= start i1)
            (<= start i2))
           (and
            (>= end i1)
            (<= end i2)))
          (setq rval nil))))
  rval)

(defun string-starts-with (str test-prefix)
  (let ((len (length test-prefix))
        (ret t))
    (if (>= (length str) len)
        (dotimes (i len)
          (if (not (equal (char str i) (char test-prefix i)))
              (let ()
                (setq ret nil)
                (return))))
        (setq ret nil))
    ret))

;;
;; utility for detecting names in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-names (words tags exclusion-list)
  (let* ((len (length words))
         word
         (ret '()))
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 4 word names:
      (if (< i (- len 3))
          ;; case #1: single element from '*name-prefix-list*'
          (if (and
               (not-in-list-find-names-helper ret i (+ i 4))
               (not-in-list-find-names-helper exclusion-list i (+ i 4))
               (member word *name-prefix-list* :test #'equal)
               (equal "." (aref words (1+ i)))
               (gethash (aref words (+ i 2)) *first-name-hash*)
               (gethash (aref words (+ i 3)) *last-name-hash*))
              (if (and
                   (string-starts-with (aref tags (+ i 2)) "NN")
                   (string-starts-with (aref tags (+ i 3)) "NN"))
                  (setq ret (cons (list i (+ i 4)) ret))))
          ;; case #1: two elements from '*name-prefix-list*'
          (if (and
               (not-in-list-find-names-helper ret i (+ i 4))
               (not-in-list-find-names-helper exclusion-list i (+ i 4))
               (member word *name-prefix-list* :test #'equal)
               (member (aref words (1+ i)) *name-prefix-list* :test #'equal)
               (gethash (aref words (+ i 2)) *first-name-hash*)
               (gethash (aref words (+ i 3)) *last-name-hash*))
              (if (and
                   (string-starts-with (aref tags (+ i 2)) "NN")
                   (string-starts-with (aref tags (+ i 3)) "NN"))
                  (setq ret (cons (list i (+ i 4)) ret)))))
      ;; process 3 word names:
      (if (< i (- len 2))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 3))
               (not-in-list-find-names-helper exclusion-list i (+ i 3)))
              (if (or
                   (and
                    (member word *name-prefix-list* :test #'equal)
                    (gethash (aref words (1+ i)) *first-name-hash*)
                    (gethash (aref words (+ i 2)) *last-name-hash*)
                    (string-starts-with (aref tags (+ i 1)) "NN")
                    (string-starts-with (aref tags (+ i 2)) "NN"))
                   (and
                    (member word *name-prefix-list* :test #'equal)
                    (member (aref words (1+ i)) *name-prefix-list* :test #'equal)
                    (gethash (aref words (+ i 2)) *last-name-hash*)
                    (string-starts-with (aref tags (+ i 1)) "NN")
                    (string-starts-with (aref tags (+ i 2)) "NN"))
                   (and
                    (member word *name-prefix-list* :test #'equal)
                    (equal "." (aref words (1+ i)))
                    (gethash (aref words (+ i 2)) *last-name-hash*)
                    (string-starts-with (aref tags (+ i 2)) "NN"))
                   (and
                    (gethash word *first-name-hash*)
                    (gethash (aref words (1+ i)) *first-name-hash*)
                    (gethash (aref words (+ i 2)) *last-name-hash*)
                    (string-starts-with (aref tags i) "NN")
                    (string-starts-with (aref tags (+ i 1)) "NN")
                    (string-starts-with (aref tags (+ i 2)) "NN")))
                  (setq ret (cons (list i (+ i 3)) ret)))))
      ;; process 2 word names:
      (if (< i (1- len))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 2))
               (not-in-list-find-names-helper exclusion-list i (+ i 2)))
              (if (or
                   (and
                    (member word '("Mr" "Mrs" "Ms" "Doctor" "President" "Premier") :test #'equal)
                    (string-starts-with (aref tags (1+ i)) "NN")
                    (gethash (aref words (1+ i)) *last-name-hash*))
                   (and
                    (gethash word *first-name-hash*)
                    (gethash (aref words (1+ i)) *last-name-hash*)
                    (string-starts-with (aref tags i) "NN")
                    (string-starts-with (aref tags (1+ i)) "NN")))
                  (setq ret (cons (list i (+ i 2)) ret)))))
      ;; 1 word names:
      (if (gethash word *last-name-hash*)
          (if (and
               (string-starts-with (aref tags i) "NN")
               (not-in-list-find-names-helper ret i (1+ i))
               (not-in-list-find-names-helper exclusion-list i (1+ i)))
              (setq ret (cons (list i (1+ i)) ret)))))
    (reverse ret)))


(defun test-names ()
  (let* ((words '#("President" "Bush" "went" "to" "San" "Diego" "to" "meet" "Ms" "." "Jones"
                   "and" "Gen" "." "Pervez" "Musharraf" "."))
         (tags (fasttag:part-of-speech-tagger words)))
    (print tags)
    (find-names words tags nil)))


;;                    FIND PLACE NAMES IN TEXT:


;; count number of places
(defun count-places (&aux (count 0))
  (maphash
   #'(lambda (key value) (setq count (1+ count)))
   place-hash)
  count)

(defun not-in-list-find-places-helper (a-list start end &aux (rval t))
  (dolist (x a-list)
    (let ((i1 (car x))
          (i2 (cadr x)))
      (if (or
           (and
            (>= start i1)
            (<= start i2))
           (and
            (>= end i1)
            (<= end i2)))
          (setq rval nil))))
  rval)


;;
;; utility for detecting place names in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-places (words exclusion-list)
  (let* ((len (length words))
         (ret '())
         word)
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 3 word place names:
      (if (< i (- len 2))
          (if (and
               (not-in-list-find-places-helper ret i (+ i 3))
               (not-in-list-find-places-helper exclusion-list i (+ i 3)))
              (let ((words (concatenate 'string word " " (aref words (1+ i)) " " (aref words (+ i 2)))))
                (if (gethash words place-hash)
                    (setq ret (cons (list i (+ i 3)) ret))))))
      ;; process 2 word place names:
      (if (< i (1- len))
          (if (and
               (not-in-list-find-places-helper ret i (+ i 2))
               (not-in-list-find-places-helper exclusion-list i (+ i 2)))
              (let ((words (concatenate 'string word " " (aref words (1+ i)))))
                (if (gethash words place-hash)
                    (setq ret (cons (list i (+ i 2)) ret))))))
      ;; 1 word place names:
      (if (and
           (not-in-list-find-places-helper ret i (+ i 1))
           (not-in-list-find-places-helper exclusion-list i (+ i 1)))
          (if (gethash word place-hash)
              (setq ret (cons (list i (1+ i)) ret)))))
    ;;(print (list "debug: places:" (reverse ret)))
    (reverse ret)))


;; test: (find-places '("President" "Bush" "went" "to" "San" "Diego"))
(defun test-places ()
  (let* ((words '#("President" "Bush" "went" "to" "France" "and" "Germany" "to" "meet" "Ms" "." "Jones")))
    (find-places words '((10 11)))))


;;
;; utility for detecting company names in a word list
;;
;;
;; argument: a list of words as strings
;; return: a list of lists. each sub-list contains a starting and ending index)
;;
(defun find-companies (words exclusion-list)
  (let* ((len (length words))
         (ret '())
         word)
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 3 word company names:
      (if (< i (- len 2))
          (if (and
               (not-in-list-find-companies-helper ret i (+ i 3))
               (not-in-list-find-companies-helper exclusion-list i (+ i 3)))
              (let ((words (concatenate 'string word " " (aref words (1+ i)) " " (aref words (+ i 2)))))
                (if (gethash words *company-name-hash*)
                    (setq ret (cons (list i (+ i 3)) ret))))))
      ;; process 2 word company names:
      (if (< i (1- len))
          (if (and
               (not-in-list-find-companies-helper ret i (+ i 2))
               (not-in-list-find-companies-helper exclusion-list i (+ i 2)))
              (let ((words (concatenate 'string word " " (aref words (1+ i)))))
                (if (gethash words *company-name-hash*)
                    (setq ret (cons (list i (+ i 2)) ret))))))
      ;; 1 word company names:
      (if (and
           (not-in-list-find-companies-helper ret i (+ i 1))
           (not-in-list-find-companies-helper exclusion-list i (+ i 1)))
          (if (gethash word *company-name-hash*)
              (setq ret (cons (list i (1+ i)) ret)))))
    ;;(print (list "debug: companies:" (reverse ret)))
    (reverse ret)))

(defun not-in-list-find-companies-helper (a-list start end &aux (rval t))
  (dolist (x a-list)
    (let ((i1 (car x))
          (i2 (cadr x)))
      (if (or
           (and
            (>= start i1)
            (<= start i2))
           (and
            (>= end i1)
            (<= end i2)))
          (setq rval nil))))
  rval)



;;                       TOP LEVEL FUNCTIONS FOR FINDING BOTH NAMES AND PLACES IN TEXT:



(defun remove-shorter-names (lst &aux (c-lst lst) (num (length lst)))
  (dotimes (i num)
    (dotimes (j num)
      (if (not (= i j))
          (let* ((s-i (nth i lst))
                 (s-j (nth j lst))
                 (tokens-i (myutils:tokenize-string s-i))
                 (tokens-j (myutils:tokenize-string s-j)))
            ;;(format t "~%* s-i: ~A s-j: ~A~%" s-i s-j)
            (if (and
                 (> (length s-i) (length s-j))
                 (> (length s-j) 6))
                (if (or
                     (search s-j s-i)
                     (let ((test t))
                       (dolist (token tokens-j)
                         (if (not (member token tokens-i :test #'equal))
                             (setf test nil)))
                       test))
                    (setf c-lst (remove s-j c-lst :test #'string-equal))))))))
  c-lst)



(defun build-list-find-name-helper (v indices)
  (let ((ret '()))
    (dolist (x indices)
      (let* ((start (car x))
             (stop (cadr x))
             (str "")
             (num (- stop start)))
        (dotimes (i num)
          (if (equal i 0)
              (setq str (concatenate 'string str (aref v (+ i start)) " "))
              (if (equal i (1- num))
                  (setq str (concatenate 'string str (aref v (+ i start))))
                  (setq str (concatenate 'string str (aref v (+ i start)) " ")))))
        (setq ret (cons (string-trim '(#\Space) str) ret))))
    (reverse ret)))


;;
;; wrapper for finding human names and place names in a text object
;;
(defun find-names-places (txt-object)
  (let* ((words (text-text txt-object))
         (tags (text-tags txt-object))
         (place-indices (find-places words nil))
         (name-indices (find-names words tags place-indices))
         (name-list (remove-duplicates (build-list-find-name-helper words name-indices) :test #'equal))
         (place-list (remove-duplicates (build-list-find-name-helper words place-indices) :test #'equal)))
    (let ((ret '()))
      (dolist (x name-list)
        (if (search " " x)
            (setq ret (cons x ret))))
      (setq name-list (reverse ret)))
    (list
     (remove-shorter-names name-list)
     (remove-shorter-names place-list))))
;; test: (find-names-places (car *all-text-objects*))

;; Test: (dolist (x *all-text-objects*) (find-names-places x))


;;                             ASSIGN CATEGRY TAGS TO TEXT:


(defvar categoryHashtables)
(defvar categoryNames)

(defun get-cat-tag-names ()
  categoryNames)

(defun get-word-list-category (words)
  (let* ((x nil)
         (ss nil)
         (cat-hash nil)
         (word nil)
         (len (length words))
         (num-categories (length categoryHashtables))
         (category-score-accumulation-array
          (make-array num-categories :initial-element 0)))

    (defun list-sort (list-to-sort)
      ;;(pprint list-to-sort)
      (sort list-to-sort
            #'(lambda (list-element-1 list-element-2)
                (> (cadr list-element-1) (cadr list-element-2)))))

    (do ((k 0 (+ k 1)))
        ((equal k len))
      (setf word (string-downcase (aref words k)))
      (do ((i 0 (+ i 1)))
          ((equal i num-categories))
        (setf cat-hash (nth i categoryHashtables))
        (setf x (gethash word cat-hash))
        (if x
            (setf 
             (aref category-score-accumulation-array i)
             (+ x (aref category-score-accumulation-array i))))))
    (setf ss '())
    (do ((i 0 (+ i 1)))
        ((equal i num-categories))
      (if (> (aref category-score-accumulation-array i) 0.01)
          (setf
           ss
           (cons
            (list
             (nth i categoryNames)
             (round (* (aref category-score-accumulation-array i) 10)))
            ss))))
    (setf ss (list-sort ss))
    (let ((cutoff (/ (or (cadar ss) 0) 2))
          (results-array '()))
      (dolist (hit ss)
        (if (> (cadr hit) cutoff)
            (setf results-array (cons hit results-array))))
      (reverse results-array))))


;; test:  (get-word-list-category (words-from-string "banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady."))




;;               SUMMARIZE TEXT:

;;
;; This function performs a simple summarization by forming a word use histogram
;; and after tossing out common words (stemmed, of course), ranking sentences
;; based on how frequently words are used in them.
;;
(defun summarize (txt-obj)
  (let* ((words (text-text txt-obj))
         (num-words (length words))
         (cats (text-category-tags txt-obj))
         (sentence-count 0)
         best-sentences sentence (score 0))
    ;; loop over sentences:
    (dotimes (i num-words)
      (let* ((word (svref words i)))
        (dolist (cat cats)
          (let* ((hash (gethash (car cat) categoryToHash))
                 (value (gethash word hash)))
            (if value
                (setq score (+ score (* 0.01 value (cadr cat)))))))
        (push word sentence)
        (if (or (equal word ".") (equal word "!") (equal word ";"))
            (let ()
              (setq sentence (reverse sentence))
              (setq score (/ score (1+ (length sentence))))
              (setq sentence-count (1+ sentence-count))
              (format t "~%~A : ~A~%" sentence score)
              ;; process this sentence:
              (if (and
                   (> score 0.4)
                   (> (length sentence) 4)
                   (< (length sentence) 30))
                  (progn
                    (setq sentence
                          (reduce
                           #'(lambda (x y) (concatenate 'string x " " y))
                           (coerce sentence 'list)))
                    (push (list sentence score) best-sentences)))
              (setf sentence nil score 0)))))
    (setf
     best-sentences
     (sort
      best-sentences
      #'(lambda (x y) (> (cadr x) (cadr y)))))
    (if best-sentences
        (replace-all
         (reduce #'(lambda (x y) (concatenate 'string x " " y))
                 (mapcar #'(lambda (x) (car x)) best-sentences))
         " ." ".")
        "<no summary>")))


;;                             TOP LEVEL NLP FUNCTION:

(defun make-text-object (words &key (url "") (title ""))
  (if (typep words 'string) (setq words (words-from-string words)))
  (let* ((txt-obj (make-text :text words :url url :title title))) ;;;  :classification cat)))
    (setf (text-tags txt-obj) (fasttag:part-of-speech-tagger words))
    ;; note: we must find human and place names before calling
    ;; pronoun-resolution:
    (let ((names-places (find-names-places txt-obj)))
      (setf (text-human-names txt-obj) (car names-places))
      (setf (text-place-names txt-obj) (cadr names-places)))
    (setf (text-company-names txt-obj) ( build-list-find-name-helper words (find-companies words '())))
    (setf (text-category-tags txt-obj)
          (mapcar #'(lambda (x) (list (car x) (/ (cadr x) 1000000.0))) (get-word-list-category (text-text txt-obj))))
    (setf (text-summary txt-obj) (summarize txt-obj))
    txt-obj))


#|

(defvar x)
(setf x (kbnlp:make-text-object "President Bill Clinton ran for president of the USA in two elections. George W Bush also ran twice. Bill Clinton took a long vacation in Europe to visit his daughter. Bill Clinton said that banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady. The question is whether or not the US dollar remains the world's reserve currency - if not, the US economy will face a depression. In their zeal to protect their members from politically hazardous votes on issues such as gay marriage and gun control, Democrats running the House of Representatives are taking extraordinary steps to muzzle Republicans in this summer's debates on spending bills.

On Thursday, for example, Republicans had hoped to force debates on abortion, school vouchers and medical marijuana, as well as gay marriage and gun control, as part of House consideration of the federal government's contribution to the District of Columbia's city budget. The group went to Florida and Alaska.

No way, Democrats said.

At issue are 12 bills totaling more than $1.2 trillion in annual appropriations bills for funding most government programs — usually low-profile legislation that typically dominates the work of the House in June and July. For decades, those bills have come to the floor under an open process that allows any member to try to amend them. Often those amendments are an effort to change government policy by adding or subtracting money for carrying it out. Relentlessly rising unemployment is triggering more home foreclosures, threatening the Obama administration's efforts to end the housing crisis and diminishing hopes the economy will rebound with vigor.

In past recessions, the housing industry helped get the economy back on track. Home builders ramped up production, expecting buyers to take advantage of lower prices and jump into the market. But not this time.

These days, homeowners who got fixed-rate prime mortgages because they had good credit can't make their payments because they're out of work. That means even more foreclosures and further declines in home values.

The initial surge in foreclosures in 2007 and 2008 was tied to subprime mortgages issued during the housing boom to people with shaky credit. That crisis has ebbed, but it has been replaced by more traditional foreclosures tied to the recession.

Unemployment stood at 9.5 percent in June " :title "test doc 1" :url "http://knowledgebooks.com/docs/001"))

(pprint x)


|#


