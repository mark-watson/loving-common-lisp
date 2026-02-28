(in-package :lw-grapher)
;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

;; A simple Grapher using the layout algorithm from the ISI-Grapher user guide

;; Some code derived from LispWorks CAPI examples drag-and-drop.lisp and circled-graph-nodes.lisp

(defclass text-node (capi:pinboard-object)
  ((text :initarg :text :reader text-node-text)
   (string-x-offset :accessor text-node-string-x-offset)
   (string-y-offset :accessor text-node-string-y-offset)))

(defmethod capi:draw-pinboard-object (pinboard (self text-node)
                                               &key &allow-other-keys)
  (multiple-value-bind (X Y  width height)
      (capi:static-layout-child-geometry self)
    (let* ((half-width  (floor (1- width)  2))
           (half-height (floor (1- height) 2))
           (circle-x (+ X half-width))
           (circle-y (+ Y half-height))
           (background :white)
           (foreground (if background
                           :black
                         (capi:simple-pane-foreground pinboard)))
           (text (text-node-text self)))
        (when background
          (gp:draw-ellipse pinboard
                           circle-x circle-y
                           half-width half-height
                           :filled t
                           :foreground background))
        (gp:draw-ellipse pinboard
                         circle-x circle-y
                         half-width half-height
                         :foreground foreground)
        (gp:draw-string pinboard
                        text
                        (+ X (text-node-string-x-offset self))
                        (+ Y (text-node-string-y-offset self))
                        :foreground foreground))))

(defun make-node (graph-pane node)
  (declare (ignorable graph-pane))
  (make-instance 'text-node :text (format nil "~A" node)))

(defmethod capi:calculate-constraints ((self text-node))
  (let* ((pl (capi:pinboard-object-pinboard self))
         (font (capi:simple-pane-font pl)))
    (multiple-value-bind
        (left top right bottom)
        (gp:get-string-extent (capi:pinboard-object-pinboard self)
                              (text-node-text self)
                              font)
      (let* ((width (+ 12 (- right left)))
             (height (+ 12 (- bottom top))))
        (setf (text-node-string-x-offset self)
              (floor (- width (- right left)) 2)
              (text-node-string-y-offset self )
              (+ (floor (- height (- bottom top)) 2)
                 (gp:get-font-ascent pl font)))
        (capi:with-geometry self
          (setf capi:%width% width 
                capi:%min-width%  width
                capi:%height% height 
                capi:%min-height% height))))))

(defvar *last-selected-node* nil)

(defun handle-mouse-click-on-pane (pane x y)
  (format t "++ handle-mouse-click-on-pane x=~a y=~a~%" x y)
  (let ((object (capi:pinboard-object-at-position pane x y)))
    (format t "---- object=~A~%" object)
    (if object
        (let ()
          (if *last-selected-node*
              (capi:unhighlight-pinboard-object pane *last-selected-node*))
          (setf *last-selected-node* object)
          (capi:highlight-pinboard-object pane object)))))

(defun graph-layout (self &key force)
  (declare (ignore force))
  (let* ((nodes (capi:graph-pane-nodes self))
         (last-y 0)
         ;;(height 30)
         ;;(width 80)
         )
    (format t "~%++++ nodes: ~A~%~%" nodes)
    (dolist (node nodes)
      (pprint node)
      (setf
       (capi:graph-node-x node) 0
       (capi:graph-node-y node) 0))
    (labels
        ((get-unlaid-out-child-nodes (a-node)
           (let (ret)
             (dolist (x (capi:graph-node-children a-node))
               (if (equal  (capi:graph-node-children x) 0)
                   (setf ret (cons x ret))))
             (reverse ret)))
         (get-average-Y-of-node-list (a-node-list)
           (let ((average 0))
             (dolist (another-node a-node-list)
               (setf average (+ average (capi:graph-node-y another-node))))
             (if (equal (length a-node-list) 0)
                 0
               (floor (/ average (length a-node-list))))))
         (get-average-X-of-node-list (a-node-list)
           (let ((average 0))
             (dolist (a-node a-node-list)
               (setf average (+ average (capi:graph-node-x a-node))))
             (if (equal (length a-node-list) 0)
                 0
               (floor (/ average (length a-node-list))))))
         (layout-Y (a-node)
           (let ((children (capi:graph-node-children a-node))
                 (children-needing-layout (get-unlaid-out-child-nodes a-node)))
             (if (equal (capi:graph-node-y a-node) 0)
                 ;; check in node n has any unlayed-out child nodes
                 (if (> (length children-needing-layout) 0)
                     (let ()
                       (dolist (child-node children-needing-layout)
                         (layout-Y child-node))
                       (setf (capi:graph-node-y a-node) (get-average-Y-of-node-list children)))
                   ;; layout a leaf node:
                   (let ()
                     (setf (capi:graph-node-y a-node) (+ last-y (capi:graph-node-height a-node) 5))
                     (setf last-y (capi:graph-node-y a-node)))))))
         (get-parents (a-node)
           (let (ret)
             (dolist (possible-parent nodes)
               (dolist (child-to-check (capi:graph-node-children possible-parent))
                 (if (equal child-to-check a-node)
                   (setf ret (cons possible-parent ret)))))
             (reverse ret)))
         (get-all-leaf-nodes ()
           (let (ret)
             (dolist (node nodes)
               (if (equal (length (capi:graph-node-children node)) 0)
                   (setf ret (cons node ret))))
             (reverse ret)))
         (layout-X (a-node)
           (if (equal (capi:graph-node-x a-node) 0)
               (let ((parents (get-parents a-node)))
                 (if (> (length parents) 0)
                     (let ((max-val 0))
                       (dolist (parent parents)
                         (layout-X parent))
                       (dolist (parent parents)
                         (let ((w (+ (capi:graph-node-x parent) (capi:graph-node-width a-node) 9)))
                           (if (> w max-val)
                               (setf max-val w))))
                       (setf (capi:graph-node-x a-node) max-val)))))))

      ;; process in Y direction
      (dolist (root nodes) ;;  (capi:graph-pane-roots self))
        (layout-Y root))
      ;; process in X direction
      (dolist (a-leaf (get-all-leaf-nodes))
        (layout-x a-leaf)))))

#|
(defun node-children (node)
  (format t "node: ~S~%" node)
  (cadr
   (assoc
    node
    '((1 (2 41))
      (2 (33 "text node"))
      (100 '(2 33))
      (555 '(100))
      (888 (41 999999000111))
      ("base-node-0" (1 "text node")))
    :test #'equal)))

(capi:contain
 (make-instance 'capi:graph-pane
                :roots '(1 100 "base-node-0" 555 888)
                :layout-function 'graph-layout
                :children-function 'node-children
                :node-pinboard-class 'text-node
                :input-model `(((:button-1 :release) ;; :press)
                                ,#'(lambda (pane x y)
                                     (handle-mouse-click-on-pane pane x y))))
                :node-pane-function 'make-text-node))
 |#
