(in-package :lw-grapher)
;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

;; A Grapher (using the layout algorithm from the ISI-Grapher user guide) with an info panel

(defun make-info-panel-grapher (h-root-name-list h-edge-list h-callback-function-click h-callback-function-shift-click)
  (format t "++  make-info-panel-grapher: h-root-name-list: ~S h-edge-list: ~S~%" h-root-name-list h-edge-list)
  (let (edges roots last-selected-node node-callback-click node-callback-click-shift output-pane)
    (labels
        ((handle-mouse-click-on-pane (pane x y)
           (ignore-errors
             (let ((object (capi:pinboard-object-at-position pane x y)))
               (if object
                   (let ()
                     (if last-selected-node
                         (capi:unhighlight-pinboard-object pane last-selected-node))
                     (setf last-selected-node object)
                     (capi:highlight-pinboard-object pane object)
                     (let ((c-stream (collector-pane-stream output-pane))) 
                       (format c-stream (funcall node-callback-click (text-node-full-text object)))
                       (terpri c-stream)))))))
         (handle-mouse-click-shift-on-pane (pane x y)
           (ignore-errors
             (let ((object (capi:pinboard-object-at-position pane x y)))
               (if object
                   (let ()
                     (if last-selected-node
                         (capi:unhighlight-pinboard-object pane last-selected-node))
                     (setf last-selected-node object)
                     (capi:highlight-pinboard-object pane object)
                     (let ((c-stream (collector-pane-stream output-pane)))
                       (format c-stream (funcall node-callback-click-shift (text-node-full-text object)))
                       (terpri c-stream)))))))
         
         (info-panel-node-children-helper (node-text)
           (format t "-- info-panel-node-children-helper node-text: ~S~%" node-text)
           (let (ret)
             (dolist (e edges)
               (if (equal (first e) node-text)
                   (setf ret (cons (second e) ret))))
             (format t "- return from info-panel-node-children-helper: ~A~%" (reverse ret))
             (reverse ret)))
         
         (make-info-panel-grapher-helper (root-name-list edge-list callback-function-click callback-function-click-shift)
           ;; example: root-name-list: '("n1") edge-list: '(("n1" "n2") ("n1" "n3"))
           (setf edges edge-list
                 roots root-name-list
                 node-callback-click callback-function-click
                 node-callback-click-shift callback-function-click-shift)
           (capi:contain 
            
            (make-instance
             'column-layout
             :title "Entity Browser"
             :description
             (list
              (make-instance 'capi:graph-pane
                             :min-height 330
                             :max-height 420
                             :roots roots
                             :layout-function 'graph-layout
                             :children-function #'info-panel-node-children-helper
                             :edge-pane-function 
                             #'(lambda(self from to)
                                 (declare (ignore self))
                                 (let ((prop-name ""))
                                   (dolist (edge edge-list)
                                     (if (and
                                          (equal from (first edge))
                                          (equal to (second edge)))
                                         (if (and (> (length edge) 2) (third edge))
                                             (let ((last-index (search "/" (third edge) :from-end t)))
                                               (if last-index
                                                   (setf prop-name (subseq (third edge) (1+ last-index)))
                                                 (setf prop-name (third edge)))))))
                                   (make-instance 
                                    'capi:labelled-arrow-pinboard-object
                                    :data (format nil "~A" prop-name))))
                             :node-pinboard-class 'text-node
                             :input-model `(((:button-1 :release)
                                             ,#'(lambda (pane x y)
                                                  (handle-mouse-click-on-pane pane x y)))
                                            ((:button-1 :release :shift) ;; :press)
                                             ,#'(lambda (pane x y)
                                                  (handle-mouse-click-shift-on-pane pane x y))))
                             :node-pane-function 'make-text-node)
              (setf
               output-pane
               (make-instance 'capi:collector-pane
                              :min-height 130
                              :max-height 220
                              :title "Message collection pane"
                              :text "..."
                              :vertical-scroll t
                              :horizontal-scroll t))))
            :title "Info Pane Browser: mouse click for info, mouse click + shift for web browser"
            
            :best-width 550 :best-height 450)))
      (make-info-panel-grapher-helper h-root-name-list h-edge-list h-callback-function-click  h-callback-function-shift-click))))


