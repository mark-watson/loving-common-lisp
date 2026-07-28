;;;; reader.lisp --- Reader macros for the NSK query syntax.
;;;;
;;;;   ?name           -> (logic-var 'name)          a logic variable
;;;;   ~pred           -> (neural-predicate 'pred)   an LLM fallback relation
;;;;   [s p o]         -> (match-triple s p o)        a triple pattern
;;;;
;;;; The macros live in their own readtable so ordinary source files keep the
;;;; standard syntax. The REPL binds *readtable* to *nsk-readtable*; other code
;;;; can call ENABLE-NSK-SYNTAX to add them to the current readtable.

(in-package :nsk)

(defvar *nsk-readtable* (copy-readtable nil)
  "A readtable that adds ?var, [triple], and ~neural syntax.")

(defun install-nsk-syntax (&optional (rt *readtable*))
  "Install the NSK reader macros into readtable RT and return it."
  ;; ?name -> (logic-var 'name); non-terminating so foo?bar stays one symbol.
  (set-macro-character #\?
    (lambda (stream char)
      (declare (ignore char))
      (list 'logic-var (list 'quote (read stream t nil t))))
    t rt)
  ;; ~pred -> (neural-predicate 'pred)
  (set-macro-character #\~
    (lambda (stream char)
      (declare (ignore char))
      (list 'neural-predicate (list 'quote (read stream t nil t))))
    t rt)
  ;; ] closes a triple exactly like ) closes a list.
  (set-macro-character #\] (get-macro-character #\) nil) nil rt)
  ;; [s p o] -> (match-triple s p o)
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignore char))
      (let ((triple (read-delimited-list #\] stream t)))
        (unless (= (length triple) 3)
          (error "NSK triple pattern needs exactly three elements: ~s" triple))
        (cons 'match-triple triple)))
    nil rt)
  rt)

(install-nsk-syntax *nsk-readtable*)

(defun enable-nsk-syntax ()
  "Copy the current *readtable* and add NSK syntax to it."
  (setf *readtable* (copy-readtable *readtable*))
  (install-nsk-syntax *readtable*))

(defun nsk-read-from-string (string)
  "Read one form from STRING using the NSK readtable."
  (let ((*readtable* *nsk-readtable*))
    (read-from-string string)))
