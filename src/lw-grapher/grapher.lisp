(in-package :lw-grapher)
;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

;; A simple Grapher using the layout algorithm from the ISI-Grapher user guide

(defun graph-layout (self &key force)
  (declare (ignore force))
  (let* ((nodes (capi:graph-pane-nodes self))
         (last-y 0)
         (horizontal-spacing 35)
         (vertical-spacing 24)
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
                     (setf (capi:graph-node-y a-node) (+ last-y (capi:graph-node-height a-node) vertical-spacing))
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
                         (let ((w (+ (capi:graph-node-x parent) (capi:graph-node-width a-node) horizontal-spacing)))
                           (if (> w max-val)
                               (setf max-val w))))
                       (setf (capi:graph-node-x a-node) max-val)))))))

      ;; process in Y direction
      (dolist (root nodes) ;;  (capi:graph-pane-roots self))
        (layout-Y root))
      ;; process in X direction
      (dolist (a-leaf (get-all-leaf-nodes))
        (layout-x a-leaf)))))

(let (edges roots)
  (defun node-children-helper (node-text)
    (format t "-- node-children-helper node-text: ~S~%" node-text)
    (let (ret)
      (dolist (e edges)
        (if (equal (first e) node-text)
            (setf ret (cons (second e) ret))))
      (format t "- return from node-children-helper: ~A~%" (reverse ret))
      (reverse ret)))
    
  (defun make-grapher (root-name-list edge-list)
    ;; example: root-name-list: '("n1") edge-list: '(("n1" "n2") ("n1" "n3"))
    (setf edges edge-list
          roots root-name-list)
    (capi:contain
     (make-instance 'capi:graph-pane
                    :roots roots
                    :layout-function 'graph-layout
                    :children-function 'node-children-helper
                    :node-pinboard-class 'text-node
                    :node-pane-function 'make-text-node)
     :best-width 300 :best-height 400)))

