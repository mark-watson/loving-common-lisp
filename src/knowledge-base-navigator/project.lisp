;;;; project.lisp
;;;;
;;;; Package definition and global variables for the Knowledge Base Navigator.

(defpackage #:knowledge-base-navigator
  (:use #:cl)
  (:export #:kbn-ui
           #:get-gemini-chat-completion))

(in-package #:knowledge-base-navigator)

;; Add any global parameters or initialization here if needed.
