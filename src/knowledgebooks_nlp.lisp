;; Copyright Mark Watson 2001-2013. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; Common Lisp stemming code in this file was written by Steven M. Haflich based on the work of Martin Porter.

(defpackage kbnlp
  (:use :cl :asdf)
  (:export
   :make-text-object)
  (:documentation
   "Mark Watson's NLP utilities released under the AGPL and Apache 2 Licenses"))

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
  text
  tags
  stems)



;;                    INITIALIZE DATA - this may take a minute or two

(print "Startng to load data....")

(defvar *male* "male")
(defvar *female* "female")
(defvar *male-or-female* "male-or-female")
(defvar *first-name-hash* (make-hash-table :test #'equal :size 4000))
(defvar *last-name-hash* (make-hash-table :test #'equal :size 4000))
(defvar *company-name-hash* (make-hash-table :test #'equal :size 400))
(defvar *product-name-hash* (make-hash-table :test #'equal :size 400))

(load "data/FastTagData")
(load "data/cat-data-tables")

(let (line)
  (with-open-file
      (in "data/names/names.male")
    (dotimes (i 50000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *first-name-hash*) *male*)))
  (with-open-file
      (in "data/names/names.female")
    (dotimes (i 50000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (if (null (gethash line *first-name-hash*))
          (setf (gethash line *first-name-hash*) *female*)
          (setf (gethash line *first-name-hash*) *male-or-female*)))))

(let (line)
  (with-open-file
      (in "data/names/names.last")
    (dotimes (i 5000000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *last-name-hash*) *male*))))

(if (not (boundp 'place-hash))
    (progn
      (load "data/names/PlaceData.ldat")))

(if (not (boundp 'stop-word-hash))
    (progn
      (load "data/names/StopWords.ldat")))

(let (line)
  (with-open-file
      (in "data/names/names.companies")
    (dotimes (i 500000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *company-name-hash*) t))))

(let (line)
  (with-open-file
      (in "data/names/names.products")
    (dotimes (i 500000)
      (setq line (read-line in nil nil))
      (if (null line) (return))
      (setf (gethash line *product-name-hash*) t))))

(print "....done loading data.")

;;;   done loading data

;; Conversion of KnowledgeBooks.com Java FastTag to Common Lisp


(defun words-from-string (str)
  (let ((ss (tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))

(defun part-of-speech-tagger (words &aux lcw r w)
  (if (typep words 'string) (setq words (words-from-string words)))
  (let* ((len (length words))
         (ret (make-array (list len)))
         lastRet
         lastWord)
    (dotimes (k len)
      (setq w (aref words k))
      (setq r (gethash w lex-hash))
      ;; if this word is not in the hash table, try making it all lower case:
      (setq lcw (string-downcase w))
      (if (null r) (setq r (gethash lcw lex-hash)))
      (if (null r)
          (let ()
            (if (or
                 (equal lcw "(")
                 (equal lcw ")")
                 (equal lcw "[")
                 (equal lcw "]")
                 (equal lcw "{")
                 (equal lcw "}"))
                (setq r (list lcw)))))
      (if (null r)
          (setq r "NN")
          (if (listp r) (setq r (car r))))
      ;; apply transformation rules:

                                        ; rule 1: DT, {VBD, VBP, VB} --> DT, NN
      (if (equal lastRet "DT")
          (if (or
               (equal r "VBD")
               (equal r "VBP")
               (equal r "VB"))
              (setq r "NN")))

                                        ; rule 2: convert a noun to a number if a "." appears in the word (but not if the token is just ".")
      (if (and (search "." w) (> (length w) 1)) (setq r "CD"))

                                        ; rule 3: convert a noun to a past participle if word ends with "ed"
      (if (equal (search "N" r) 0)
          (let ((i (search "ed" w :from-end t)))
            (if (equal i (- (length w) 2))
                (setq r "VBN"))))

                                        ; rule 4: convert any type to an adverb if it ends with "ly"
      (let ((i (search "ly" w :from-end t)))
        (if (equal i (- (length w) 2))
            (setq r "RB")))

                                        ; rule 5: convert a common noun (NN or NNS) to an adjective
                                        ;         if it ends with "al"
      (if (or
           (equal r "NN")
           (equal r "NNS"))
          (let ((i (search "al" w :from-end t)))
            (if (equal i (- (length w) 2))
                (setq r "RB"))))

                                        ; rule 6: convert a noun to a verb if the receeding word is "would"
      (if (equal (search "NN" r) 0)
          (if (equal lastWord "would")
              (setq r "VB")))

                                        ; rule 7: if a word has been categorized as a common noun and it
                                        ;         ends with "s", then set its type to a plural noun (NNS)
      (if (equal r "NN")
          (let ((i (search "s" w :from-end t)))
            (if (equal i (- (length w) 1))
                (setq r "NNS"))))

                                        ; rule 8: convert a common noun to a present participle verb
                                        ;         (i.e., a gerand)
      (if (equal (search "NN" r) 0)
          (let ((i (search "ing" w :from-end t)))
            (if (equal i (- (length w) 3))
                (setq r "VBG"))))

      (setq lastRet ret)
      (setq lastWord w)
      (setf (aref ret k) r))
    ret))


;; (part-of-speech-tagger #("the" "cat" "ran"))
;; (part-of-speech-tagger "President Bush went to China. He wanted a good trade agreement.")

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
         (tags (part-of-speech-tagger words)))
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



;;                       TOP LEVEL FUNCTIONS FOR FINDING BOTH NAMES AND PLACES IN TEXT:



(defun remove-shorter-names (lst &aux (c-lst lst) (num (length lst)))
  (dotimes (i num)
    (dotimes (j num)
      (if (not (= i j))
          (let* ((s-i (nth i lst))
                 (s-j (nth j lst))
                 (tokens-i (tokenize-string s-i))
                 (tokens-j (tokenize-string s-j)))
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




;;                     STEM TEXT:

;; Note that only lower case sequences are stemmed. Forcing to lower case
;; should be done before stem(...) is called.

;;
;; wrapper for steming text using the KBtextmaster 'text' object as the input
;; I believe that this code is in the public domain. This Common Lisp port of
;; the porter stemmer was written by Steven M. Haflich.

(defun stem-text (txt-object)
  (let* ((words (text-text txt-object))
         (len (length words))
         (stems (make-array (list len))))
    (dotimes (i len)
      (let* ((word (aref words i))
             (st (stem (string-downcase word))))
        (setf (aref stems i) st)))
    stems))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Porter Stemming Algorithm, somewhat mechanically hand translated to Common Lisp by
;; Steven M. Haflich smh@franz.com Feb 2002.  Most of the inline comments refer to the
;; original C code.  At the time of this translation the code passes the associated Porter
;; test files.  See the function test at the end of this file.

;; This port is intended to be portable ANSI Common Lisp.  However, it has only been
;; compiled and tested with Allegro Common Lisp.  This code is offered in the hope it will
;; be useful, but with no warranty of correctness, suitability, usability, or anything
;; else.  The C implementation from which this code was derived was not reentrant, relying
;; on global variables.  This implementation corrects that.  It is intended that a word to
;; be stemmed will be in a string with fill-pointer, as this is a natural result when
;; parsing user input, web scraping, whatever.  If not, a string with fill-pointer is
;; created, but this is an efficiency hit and is here intended only for lightweight use or
;; testing.  Using some resource mechanism on these strings would be a useful improvement,
;; whether here or in the calling code.

;; Postscript: When I contacted Martin Porter about this anachronism, he decided to fix
;; the C version to implement proper reentrancy.  The CL version is now also served from
;; his central site.  It should be functionally identical to this one, modulo the current
;; comment and a couple harmless formatting and comment changes.
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the Porter stemming algorithm, coded up in ANSI C by the
;; author. It may be be regarded as cononical, in that it follows the
;; algorithm presented in

;; Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
;; no. 3, pp 130-137,

;; only differing from it at the points maked --DEPARTURE-- below.

;; See also http://www.tartarus.org/~martin/PorterStemmer

;; The algorithm as described in the paper could be exactly replicated
;; by adjusting the points of DEPARTURE, but this is barely necessary,
;; because (a) the points of DEPARTURE are definitely improvements, and
;; (b) no encoding of the Porter stemmer I have seen is anything like
;; as exact as this version, even with the points of DEPARTURE!

;; You can compile it on Unix with 'gcc -O3 -o stem stem.c' after which
;; 'stem' takes a list of inputs and sends the stemmed equivalent to
;; stdout.

;; The algorithm as encoded here is particularly fast.

;; Release 1

;; The main part of the stemming algorithm starts here. b is a buffer
;; holding a word to be stemmed. The letters are in b[k0], b[k0+1] ...
;; ending at b[k]. In fact k0 = 0 in this demo program. k is readjusted
;; downwards as the stemming progresses. Zero termination is not in fact
;; used in the algorithm.

;; Note that only lower case sequences are stemmed. Forcing to lower case
;; should be done before stem(...) is called.

;; cons(i) is TRUE <=> b[i] is a consonant.

        ;;; Common Lisp port Version 1.01

        ;;;
        ;;; Common Lisp port Version history
        ;;;
        ;;; 1.0  -- smh@franz.com Feb 2002
        ;;;         initial release
        ;;;
        ;;; 1.01 -- smh@franz.com 25 Apr 2004
        ;;;         step4 signalled error for "ion" "ions".  Thanks to Jeff Heard
        ;;;         for detecting this and suggesting the fix.


;; cons(i) is TRUE <=> b[i] is a consonant.

(defun consonantp (str i)
  (let ((char (char str i)))
    (cond ((member char '(#\a #\e #\i #\o #\u)) nil)
          ((eql char #\y)
           (if (= i 0) t (not (consonantp str (1- i)))))
          (t t))))

;; m() measures the number of consonant sequences between k0 and j. if c is
;; a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
;; presence,

;;    <c><v>       gives 0
;;    <c>vc<v>     gives 1
;;    <c>vcvc<v>   gives 2
;;    <c>vcvcvc<v> gives 3
;;    ....

(defun m (str lim)
  (let ((n 0)
        (i 0))
    (loop
       (when (>= i lim) (return-from m n))
       (if (not (consonantp str i)) (return nil))
       (incf i))
    (incf i)
    (loop
       (loop
          (if (>= i lim) (return-from m n))
          (if (consonantp str i) (return nil))
          (incf i))
       (incf i)
       (incf n)
       (loop
          (if (>= i lim) (return-from m n))
          (if (not (consonantp str i)) (return nil))
          (incf i))
       (incf i))))

;; vowelinstem() is TRUE <=> k0,...j contains a vowel

(defun vowelinstem (str)
  (loop for i from 0 below (fill-pointer str)
     unless (consonantp str i) return t))

;; doublec(j) is TRUE <=> j,(j-1) contain a double consonant.

(defun doublec (str i)
  (cond ((< i 1) nil)
        ((not (eql (char str i) (char str (1- i)))) nil)
        (t (consonantp str i))))

;; cvc(i) is TRUE <=> i-2,i-1,i has the form consonant - vowel - consonant
;; and also if the second c is not w,x or y. this is used when trying to
;; restore an e at the end of a short word. e.g.

;;    cav(e), lov(e), hop(e), crim(e), but
;;    snow, box, tray.

(defun cvc (str lim)
  (decf lim)
  (if (or (< lim 2)
          (not (consonantp str lim))
          (consonantp str (1- lim))
          (not (consonantp str (- lim 2))))
      (return-from cvc nil))
  (if (member (char str lim) '(#\w #\x #\y)) (return-from cvc nil))
  t)

;; ends(s) is TRUE <=> k0,...k ends with the string s.

(defun ends (str ending)
  (declare (string str) (simple-string ending))
  (let ((len1 (length str)) (len2 (length ending)))
    (loop
       for pa downfrom (1- len1) to 0
       and pb downfrom (1- len2) to 0
       unless (eql (char str pa) (char ending pb))
       return nil
       finally (return (when (< pb 0)
                         (decf (fill-pointer str) len2)
                         t)))))

;; setto(s) sets (j+1),...k to the characters in the string s, readjusting k.

(defun setto (str suffix)
  (declare (string str) (simple-string suffix))
  (loop for char across suffix
     do (vector-push-extend char str)))

;; r(s) is used further down.

(defun r (str s sfp)
  (if (> (m str (fill-pointer str)) 0)
      (setto str s)
      (setf (fill-pointer str) sfp)))

;; step1ab() gets rid of plurals and -ed or -ing. e.g.

;;     caresses  ->  caress
;;     ponies    ->  poni
;;     ties      ->  ti
;;     caress    ->  caress
;;     cats      ->  cat

;;     feed      ->  feed
;;     agreed    ->  agree
;;     disabled  ->  disable

;;     matting   ->  mat
;;     mating    ->  mate
;;     meeting   ->  meet
;;     milling   ->  mill
;;     messing   ->  mess

;;     meetings  ->  meet

(defun step1ab (str)
  (when (eql (char str (1- (fill-pointer str))) #\s)
    (cond ((ends str "sses") (incf (fill-pointer str) 2))
          ((ends str "ies")  (setto str "i"))
          ((not (eql (char str (- (fill-pointer str) 2)) #\s)) (decf (fill-pointer str)))))
  (cond ((ends str "eed") (if (> (m str (fill-pointer str)) 0)
                              (incf (fill-pointer str) 2)
                              (incf (fill-pointer str) 3)))
        ((let ((sfp (fill-pointer str)))
           (if (or (ends str "ed")
                   (ends str "ing"))
               (if (vowelinstem str)
                   t
                   (progn (setf (fill-pointer str) sfp)
                          nil))))
         (cond ((ends str "at") (setto str "ate"))
               ((ends str "bl") (setto str "ble"))
               ((ends str "iz") (setto str "ize"))
               ((doublec str (1- (fill-pointer str)))
                (unless (member (char str (1- (fill-pointer str))) '(#\l #\s #\z))
                  (decf (fill-pointer str))))
               (t (if (and (= (m str (fill-pointer str)) 1)
                           (cvc str (fill-pointer str)))
                      (setto str "e"))))))
  str)

;; step1c() turns terminal y to i when there is another vowel in the stem.

(defun step1c (str)
  (let ((saved-fill-pointer (fill-pointer str)))
    (when (and (ends str "y")
               (vowelinstem str))
      (setf (char str (fill-pointer str)) #\i))
    (setf (fill-pointer str) saved-fill-pointer))
  str)

;; step2() maps double suffices to single ones. so -ization ( = -ize plus
;; -ation) maps to -ize etc. note that the string before the suffix must give
;; m() > 0.

(defun step2 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)
      (block nil
        (case (char str (- (length str) 2))
          (#\a (when (ends str "ational") (r str "ate"  sfp)  (return))
               (when (ends str "tional")  (r str "tion" sfp) (return)))
          (#\c (when (ends str "enci")    (r str "ence" sfp) (return))
               (when (ends str "anci")    (r str "ance" sfp) (return)))
          (#\e (when (ends str "izer")    (r str "ize"  sfp)  (return)))
          (#\l (when (ends str "bli")     (r str "ble"  sfp)  (return))
               ;; -DEPARTURE-
               ;; To match the published algorithm, replace prev line with
               ;; ((when (ends str "abli")    (r str "able" sfp) (return))
               (when (ends str "alli")    (r str "al"  sfp)   (return))
               (when (ends str "entli")   (r str "ent" sfp)  (return))
               (when (ends str "eli")     (r str "e"   sfp)    (return))
               (when (ends str "ousli")   (r str "ous" sfp)  (return)))
          (#\o (when (ends str "ization") (r str "ize" sfp)  (return))
               (when (ends str "ation")   (r str "ate" sfp)  (return))
               (when (ends str "ator")    (r str "ate" sfp)  (return)))
          (#\s (when (ends str "alism")   (r str "al"  sfp)   (return))
               (when (ends str "iveness") (r str "ive" sfp)  (return))
               (when (ends str "fulness") (r str "ful" sfp)  (return))
               (when (ends str "ousness") (r str "ous" sfp)  (return)))
          (#\t (when (ends str "aliti")   (r str "al"  sfp)   (return))
               (when (ends str "iviti")   (r str "ive" sfp)  (return))
               (when (ends str "biliti")  (r str "ble" sfp)  (return)))
          ;; -DEPARTURE-
          ;; To match the published algorithm, delete next line.
          (#\g (when (ends str "logi")    (r str "log" sfp)  (return)))))))
  str)

;; step3() deals with -ic-, -full, -ness etc. similar strategy to step2.

(defun step3 (str)
  (let ((sfp (fill-pointer str)))
    (block nil
      (case (char str (1- (length str)))
        (#\e (when (ends str "icate") (r str "ic" sfp) (return))
             (when (ends str "ative") (r str "" sfp)   (return)) ; huh?
             (when (ends str "alize") (r str "al" sfp) (return)))
        (#\i (when (ends str "iciti") (r str "ic" sfp) (return)))
        (#\l (when (ends str "ical")  (r str "ic" sfp) (return))
             (when (ends str "ful")   (r str "" sfp)   (return))) ; huh?
        (#\s (when (ends str "ness")  (r str "" sfp)   (return))) ; huh?
        )))
  str)

;; step4() takes off -ant, -ence etc., in context <c>vcvc<v>.

(defun step4 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)                     ; Unnecessary?
      (block nil
        (case (char str (- sfp 2))
          (#\a (if (ends str "al")    (return)))
          (#\c (if (ends str "ance")  (return))
               (if (ends str "ence")  (return)))
          (#\e (if (ends str "er")    (return)))
          (#\i (if (ends str "ic")    (return)))
          (#\l (if (ends str "able")  (return))
               (if (ends str "ible")  (return)))
          (#\n (if (ends str "ant")   (return))
               (if (ends str "ement") (return))
               (if (ends str "ment")  (return))
               (if (ends str "ent")   (return)))
            ;;;;(#\o (if (ends str "ion")
          (#\o (if (and (> (length str) 3) (ends str "ion"))  ;; MLW 6/19/2002
                   (if (let ((c (char str (1- (length str)))))
                         (or (eql c #\s) (eql c #\t)))
                       (return)
                       (setf (fill-pointer str) sfp)))
               (if (ends str "ou")    (return))) ; takes care of -ous
          (#\s (if (ends str "ism")   (return)))
          (#\t (if (ends str "ate")   (return))
               (if (ends str "iti")   (return)))
          (#\u (if (ends str "ous")   (return)))
          (#\v (if (ends str "ive")   (return)))
          (#\z (if (ends str "ize")   (return))))
        (return-from step4 str))
      (unless (> (m str (fill-pointer str)) 1)
        (setf (fill-pointer str) sfp)))
    str))

;; step5() removes a final -e if m() > 1, and changes -ll to -l if m() > 1.

(defun step5 (str)
  (let ((len (fill-pointer str)))
    (if (eql (char str (1- len)) #\e)
        (let ((a (m str len)))
          (if (or (> a 1)
                  (and (= a 1)
                       (not (cvc str (1- len)))))
              (decf (fill-pointer str))))))
  (let ((len (fill-pointer str)))
    (if (and (eql (char str (1- len)) #\l)
             (doublec str (1- len))
             (> (m str len) 1))
        (decf (fill-pointer str))))
  str)

;;     code for main 'stem' function

;;  Main stemming function: everything else s lexically scoped:
(defun stem (str)  ;; code for functon is at the end of this file

  ;; In stem(p,i,j), p is a char pointer, and the string to be stemmed is from p[i] to p[j]
  ;; inclusive. Typically i is zero and j is the offset to the last character of a string,
  ;; (p[j+1] == '\0'). The stemmer adjusts the characters p[i] ... p[j] and returns the new
  ;; end-point of the string, k.  Stemming never increases word length, so i <= k <= j. To
  ;; turn the stemmer into a module, declare 'stem' as extern, and delete the remainder of
  ;; this file.

  ;; With this line, strings of length 1 or 2 don't go through the
  ;; stemming process, although no mention is made of this in the
  ;; published algorithm. Remove the line to match the published
  ;; algorithm.
  (let ((len (length str)))
    (if (<= len 2) (return-from stem str)) ; /*-DEPARTURE-*/
    (if (typep str 'simple-string)      ; Primarily for testing.
        (setf str
              (make-array len :element-type 'character
                          :fill-pointer len :initial-contents str)))
    (step1ab str) (step1c str) (step2 str) (step3 str) (step4 str) (step5 str)
    str))



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
    (let ((cutoff (/ (cadar ss) 2))
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
    (setf (text-tags txt-obj) (part-of-speech-tagger words))
    (setf (text-stems txt-obj) (stem-text txt-obj))
    ;; note: we must find human and place names before calling
    ;; pronoun-resolution:
    (let ((names-places (find-names-places txt-obj)))
      (setf (text-human-names txt-obj) (car names-places))
      (setf (text-place-names txt-obj) (cadr names-places)))
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
