;; Copyright Mark Watson 2001-2013. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; Common Lisp stemming code in this file was written by Steven M. Haflich based on the work of Martin Porter.

(defpackage fasttag
  ;;(:use :cls :asdf)
  (:export
   :part-of-speech-tagger)
  (:documentation
   "Mark Watson's NLP utilities released under the AGPL and Apache 2 Licenses"))

(in-package :fasttag)

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(defvar *current-directory* (subseq a1 3 (- (length a1) 13)))

(load (concatenate 'string *current-directory* "linguistic_data/FastTagData"))
(defun words-from-string (str)
  (let ((ss (myutils:tokenize-string str)))
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


;; (fasttag:part-of-speech-tagger #("the" "cat" "ran"))
;; (fasttag:part-of-speech-tagger "President Bush went to China. He wanted a good trade agreement.")
