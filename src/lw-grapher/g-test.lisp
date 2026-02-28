(in-package :lw-grapher)


(defun t1 ()
  (make-grapher '("n1") '(("n1" "n2") ("n1" "n3"))))


(defun test-callback-2 (selected-node-name)
  (format nil "* user clicked on node: ~A~%" selected-node-name))

(defun t2 ()
  (make-info-panel-grapher '("n1") '(("n1" "n2") ("n1" "n3")) 'test-callback-2))

