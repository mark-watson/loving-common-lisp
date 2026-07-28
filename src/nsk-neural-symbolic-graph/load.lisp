;;;; load.lisp --- Load NSK into a running lw image for development.
;;;;
;;;; Usage:
;;;;   echo '(progn (load "load.lisp") (nsk:repl))' | lw
;;;;
;;;; This loads the source files directly (compiling in memory), which needs no
;;;; ASDF cache on disk. For a production build see nsk.asd and build.lisp.

(in-package :cl-user)

(defparameter *nsk-source-files*
  '("packages" "json" "unify" "store" "reader" "neural" "query" "repl" "server" "main"))

(let ((src (merge-pathnames
            "src/"
            (make-pathname :name nil :type nil
                           :defaults (or *load-truename* *load-pathname*)))))
  ;; One compilation unit defers undefined-function reports to the end, so a
  ;; forward reference between these files stays quiet. A missing function still
  ;; warns. This keeps SBCL as quiet as LispWorks on load.
  (with-compilation-unit (:override t)
    (dolist (name *nsk-source-files*)
      (load (merge-pathnames (concatenate 'string name ".lisp") src)))))

(format t "~&NSK loaded. Try (nsk:repl), or load tests/tests.lisp to run tests.~%")
