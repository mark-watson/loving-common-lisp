(in-package :lw-grapher)
;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

;; A text node class for use with simple Grapher and info panel Grapher

(defclass text-node (capi:pinboard-object)
  ((text :initarg :text :reader text-node-text)
   (full-text :initarg :full-text :reader text-node-full-text)
   (string-x-offset :accessor text-node-string-x-offset)
   (string-y-offset :accessor text-node-string-y-offset)))

(defmethod capi:draw-pinboard-object (pinboard (self text-node) ;; code derived from LispWorks CAPI examples
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

(defun make-text-node (graph-pane node)
  (declare (ignorable graph-pane))
  (let* ((node-name (format nil "~A" node))
         (last-index (search "/" node :from-end t)))
    (if last-index
        (make-instance 'text-node :text (myutils:replace-all (myutils:replace-all (subseq node-name (1+ last-index)) "_" " ") ">" "") :full-text node-name)
        (make-instance 'text-node :text node-name :full-text node-name))))

(defmethod capi:calculate-constraints ((self text-node))  ;; code derived from LispWorks CAPI examples
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
