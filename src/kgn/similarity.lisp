;; tools for ranking similarity of any two things

(in-package #:kgn)

(defun compare-name-to-uri (name uri)
  ;; Assume URI is in the form <http://dbpedia.org/resource/United_States_Senate_career_of_Barack_Obama>
  (let* ((last-slash-index (or (search "/" uri :from-end t) 0))
         (uri-suffix (remove #\Space (subseq uri (+ last-slash-index 1))))
         (cleaned-uri-suffix (remove #\/ uri))
         (squashed-name (string-downcase (remove #\Space name)))
         (score (float (/ (length (intersection (coerce cleaned-uri-suffix 'list) (coerce squashed-name 'list))) (length uri-suffix)))))
    (print (list uri-suffix squashed-name score))
    score))

(defun ta ()
  (compare-name-to-uri "Barack Obama" "<http://dbpedia.org/resource/United_States_Senate_career_of_Barack_Obama>")
  (compare-name-to-uri "Barack Obama" "<http://dbpedia.org/resource/Barack_Obama>"))
        
         