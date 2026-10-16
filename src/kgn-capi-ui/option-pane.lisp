(in-package #:kgn-capi-ui)

;; options for:
;;  1. programming language to generate code snippets for
;;  2. colorization options (do we really need this??)
;;  3. show disk space used by caching
;;  4. option to remove local disk cache

(defvar *width-options-panel* 800)

;; The SPARQL query cache is the SQLite file written by the sparql-cache package.
(defun get-cache-disk-space ()
  (let ((x (ignore-errors (floor (with-open-file (file sparql-cache::*db-path*) (file-length file))
                                 1000))))
    (or x  0))) ;; units in kilobytes

(defun clear-cache-callback (&rest val)
  (declare (ignore val))
  (ignore-errors (delete-file sparql-cache::*db-path*)))

(defvar *code-snippet-language* nil)
(defun set-prog-lang (&rest val)
  (format t "* set-prog-lang: val=~S~%" val)
  (setf *code-snippet-language* (first val)))

(capi:define-interface options-panel-interface ()
  ()
  (:panes
   #|
   (prog-lang-pane
    capi:option-pane
    :items '("No language set" "Python" "Common Lisp")
    :visible-items-count 6
    :selection (if (equal *code-snippet-language* nil)
                   0
                 (if (equal *code-snippet-language* "No language set")
                     0
                   (if (equal *code-snippet-language* "Python")
                       1
                     (if (equal *code-snippet-language* "Common Lisp")
                         2
                       0))))
    :interaction :single-selection
    :selection-callback
    'set-prog-lang)|#
   (disk-space-pane
     capi:text-input-pane
    :text (format nil "~A (kilobytes)" (get-cache-disk-space))
    :title "Current size of cache:"
    :min-width 170
    :max-width *width-options-panel*)
   (clear-disk-cache-pane
    capi:push-button-panel
    ;;:title "Clear local query cache:"
    :items 
    '("Clear local query cache")
    :selection-callback 
    #'(lambda (&rest val)
        (declare (ignore val))
        (clear-cache-callback)
        ;; sparql-cache creates its tables when loaded, so recreate them:
        (ignore-errors (sparql-cache::create-dbpedia))
        (ignore-errors (sparql-cache::create-wikidata))
        (ignore-errors (setf (capi:text-input-pane-text disk-space-pane) "0 (kilobytes)"))))
   (toggle-graph-display
    capi:option-pane
    :items '("Show Graph Info Pane Browser" "Hide Graph Info Pane Browser")
    :selected-item (if *show-info-pane* 0 1)
    ;;:title ""
    :selection-callback 'toggle-grapher-visibility))

  (:layouts
   (main-layout
    capi:grid-layout
    '(nil disk-space-pane
    nil clear-disk-cache-pane)
    :x-ratios '(1 99)
    :has-title-column-p nil))
  (:default-initargs
   :layout 'main-layout
   :title "Knowledge Graph Navigator Options"
   :max-width *width-options-panel*))

;; MAIN entry point for application:


;;         (capi:display (make-instance 'options-panel-interface))

(defun ui2 () (capi:display (make-instance 'options-panel-interface)))
