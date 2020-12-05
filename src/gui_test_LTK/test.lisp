(ql:quickload "ltk")
(in-package :ltk)
(load "ltk-widgets.lisp")
(load "ltk-mw.lisp")

(setq *wish-pathname* "/usr/bin/wish") ;; otherwise Python/conda version is used

(defun test ()
  (with-ltk ()
   (let* ((f (make-instance 'frame))
          (b1 (make-instance 'button
                             :master f
                             :text "Button 1"
                             :command (lambda () (format t "Button1~&"))))
          (b2 (make-instance 'button
                             :master f
                             :text "Button 2"
                             :command (lambda () (format t "Button2~&")))))
     (pack f)
     (pack b1 :side :left)
     (pack b2 :side :left)
     (configure f :borderwidth 3)
     (configure f :relief :sunken)
     )))
(test)
