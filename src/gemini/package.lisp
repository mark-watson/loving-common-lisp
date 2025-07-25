;;;; package.lisp

(defpackage #:gemini
  (:use #:cl)
  (:import-from #:dexador
                #:post)  ; Only import the symbols we need
  (:export #:generate #:count-tokens #:send-chat-message #:generate-streaming))
