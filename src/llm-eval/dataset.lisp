;;;; dataset.lisp -- golden datasets stored as Lisp data.

(in-package #:llm-eval)

(defstruct eval-case
  id          ; short stable name, used to match results across runs
  question
  reference   ; a correct answer, written by a person
  keywords)   ; facts a correct answer must mention

(defun load-cases (pathname)
  "Read eval cases from PATHNAME, a file of plists. *READ-EVAL* is NIL so
a #. form in a data file can't run code."
  (with-open-file (in pathname)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (loop for form = (read in nil in)
              until (eq form in)
              collect (destructuring-bind (&key id question reference keywords)
                          form
                        (unless (and (stringp id) (stringp question) (stringp reference))
                          (error "Bad eval case, needs string :id, :question, and :reference: ~S"
                                 form))
                        (make-eval-case :id id :question question
                                        :reference reference :keywords keywords)))))))
