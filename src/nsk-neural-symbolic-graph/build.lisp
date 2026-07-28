;;;; build.lisp --- Build the standalone `nsk` executable.
;;;;
;;;; LispWorks:
;;;;   lw -build build.lisp
;;;;   (produces ./nsk; needs a LispWorks with delivery)
;;;;
;;;; SBCL:
;;;;   sbcl --script build.lisp
;;;;   (produces ./nsk via save-lisp-and-die)

(in-package :cl-user)

(load (merge-pathnames "load.lisp"
                       (or *load-truename* *load-pathname*)))

#+lispworks
(progn
  ;; DELIVER writes a standalone console application. :multiprocessing is on so
  ;; hunchentoot can run under --serve.
  (funcall (find-symbol "DELIVER" :lispworks)
           'nsk:main "nsk" 0
           :console t
           :multiprocessing t
           :keep-symbols t))

#+sbcl
(sb-ext:save-lisp-and-die "nsk"
                          :toplevel #'nsk:main
                          :executable t
                          :compression t)
