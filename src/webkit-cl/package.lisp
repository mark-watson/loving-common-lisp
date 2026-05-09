;;; package.lisp — Package definition for webkit-cl

(defpackage #:webkit-cl
  (:use #:cl)
  (:export
   ;; App lifecycle
   #:with-app
   #:make-app
   #:app-run
   #:app-quit
   #:app-destroy
   ;; Content loading
   #:load-html
   #:load-url
   #:load-file
   ;; JavaScript evaluation
   #:eval-js
   ;; Bridge
   #:register-handler
   #:unregister-handler
   ;; Window management
   #:set-title
   #:set-size
   #:set-resizable
   ;; App accessors
   #:app-handle
   #:*current-app*))
