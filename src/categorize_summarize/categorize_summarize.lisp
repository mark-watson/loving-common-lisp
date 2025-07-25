;; Copyright Mark Watson 2001-2022. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; Common Lisp stemming code in this file was written by Steven M. Haflich based on the work of Martin Porter.

(defpackage categorize_summarize
  ;;(:use :cls :asdf)
  (:export
   :summarize
   :categorize
   :get-cat-tag-names)
  (:documentation
   "Mark Watson's NLP utilities released under the AGPL and Apache 2 Licenses"))

(in-package :categorize_summarize)

;; data structures

(defstruct text
  url
  title
  summary
  category-tags
)


;;                    INITIALIZE DATA - this may take a minute or two

(print "Starting to load data....")

(defvar *base-pathname-cs* #.(or *compile-file-truename* *load-truename*))
(defvar a1-cs (write-to-string *base-pathname-cs*))
(defvar *current-directory-cs* (subseq a1-cs 3 (- (length a1-cs) 26)))

(load (concatenate 'string *current-directory-cs* "/linguistic_data/cat-data-tables"))

(print "....done loading data.")

;;;   done loading data



;;                             ASSIGN CATEGRY TAGS TO TEXT:


(defvar categoryHashtables)
(defvar categoryNames)

(defun get-cat-tag-names ()
  categoryNames)

(defun categorize (words)
  (let* ((x nil)
         (ss nil)
         (cat-hash nil)
         (word nil)
         (len (length words))
         (num-categories (length categoryHashtables))
         (category-score-accumulation-array
          (make-array num-categories :initial-element 0)))

    (labels
        ((list-sort (list-to-sort)
           ;;(pprint list-to-sort)
           (sort list-to-sort
                 #'(lambda (list-element-1 list-element-2)
                     (> (cadr list-element-1) (cadr list-element-2))))))

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
      (setf ss (remove-duplicates ss :test #'equal)) ;; new bug? not sure why this is now required.
      (print (list "______ categorize  ss:" ss))
      (setf ss (list-sort ss))
      (let ((cutoff (/ (or (cadar ss) 0) 1.25))
            (results-array '()))
        (print (list "______ cutoff=" cutoff))
        (dolist (hit ss)
          (if (> (cadr hit) cutoff)
              (setf results-array (cons hit results-array))))
        (reverse results-array)))))


;;               SUMMARIZE TEXT:

;;
;; This function performs a simple summarization by forming a word use histogram
;; and after tossing out common words (stemmed, of course), ranking sentences
;; based on how frequently words are used in them.
;;
(defun summarize (words cats)
  (let* ((num-words (length words))
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
              ;;;;(format t "~%~A : ~A~%" sentence score)
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
    ;;(print (list "-- best sentences:" best-sentences))
    (if best-sentences
        (myutils:replace-all
         (reduce #'(lambda (x y) (concatenate 'string x " " y))
                 (mapcar #'(lambda (x) (car x)) best-sentences))
         " ." ".")
        "<no summary>")))

#|

(defvar s1 "Plunging European stocks, wobbly bonds and grave concerns about
the health of Portuguese lender Banco Espirito Santo SA made last
week feel like a rerun of the euro crisis, but most investors say
it was no more than a blip for a resurgent region. Banco Espirito
Santo has been in investorsa sights since December, when The Wall
Street Journal first reported on accounting irregularities at the
complex firm. Nerves frayed on Thursday when Banco Espirito Santo's
parent company said it wouldn't be able to meet some short-term debt
obligations. B J Cole gave a concert at IBM headquarters in Canada and then in France.
I heard him on the Australian Broadcasting Corporation being
critical of Australian Broadcasting Corporation.
Story was written by Frank Munoz a member of the Australian Writers Guild
as taught at the American University.")

(defvar s2 (concatenate 'string s1 " " s1))
(defvar s3 (concatenate 'string s1 " " s2))

(defun test0a ()
  (format nil "test normalization on length of text~%")
  (defun helper0a (s)
    (print (list "******* categories:" (categorize s))))
  (helper0a (myutils:words-from-string s1))
  (helper0a (myutils:words-from-string s2))
  (helper0a (myutils:words-from-string s3)))




(defvar x "President Bill Clinton ran for president of the USA in two elections. George W Bush also ran twice. Bill Clinton took a long vacation in Europe to visit his daughter. Bill Clinton said that banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady. The question is whether or not the US dollar remains the world's reserve currency - if not, the US economy will face a depression. In their zeal to protect their members from politically hazardous votes on issues such as gay marriage and gun control, Democrats running the House of Representatives are taking extraordinary steps to muzzle Republicans in this summer's debates on spending bills.

On Thursday, for example, Republicans had hoped to force debates on abortion, school vouchers and medical marijuana, as well as gay marriage and gun control, as part of House consideration of the federal government's contribution to the District of Columbia's city budget. The group went to Florida and Alaska.

No way, Democrats said.

At issue are 12 bills totaling more than $1.2 trillion in annual appropriations bills for funding most government programs — usually low-profile legislation that typically dominates the work of the House in June and July. For decades, those bills have come to the floor under an open process that allows any member to try to amend them. Often those amendments are an effort to change government policy by adding or subtracting money for carrying it out. Relentlessly rising unemployment is triggering more home foreclosures, threatening the Obama administration's efforts to end the housing crisis and diminishing hopes the economy will rebound with vigor.

In past recessions, the housing industry helped get the economy back on track. Home builders ramped up production, expecting buyers to take advantage of lower prices and jump into the market. But not this time.

These days, homeowners who got fixed-rate prime mortgages because they had good credit can't make their payments because they're out of work. That means even more foreclosures and further declines in home values.

The initial surge in foreclosures in 2007 and 2008 was tied to subprime mortgages issued during the housing boom to people with shaky credit. That crisis has ebbed, but it has been replaced by more traditional foreclosures tied to the recession.

Unemployment stood at 9.5 percent in June ")

(defvar words1 (myutils:words-from-string x))

(print words1)

(setq cats1 (categorize words1))

(print cats1)

(defvar sum1 (summarize words1 cats1))

(print sum1)


|#
