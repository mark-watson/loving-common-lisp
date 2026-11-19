;;;; package.lisp -- the :mac package shared by all files in this directory.
;;;; Load this file first.

(defpackage :mac
  (:use :cl)
  (:export
   ;; osascript.lisp
   #:osascript-error
   #:osascript-error-code
   #:osascript-error-message
   #:osascript-error-script
   #:run-osascript
   #:mail-unread-count
   #:get-todays-events
   #:send-imessage
   #:add-reminder
   ;; read-recent-imessages.lisp
   #:read-recent-imessages
   ;; safari-login.lisp
   #:safari-login))
