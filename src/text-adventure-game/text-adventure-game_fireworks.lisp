;;;; text-adventure-game_fireworks.lisp
;;;; Text adventure game using Fireworks AI for AI-driven storytelling.
;;;; Model access goes through the litelm routing library (../litelm).
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game_fireworks.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game_fireworks.lisp --eval '(text-adventure:play)'
;;;;
;;;; Requires FIREWORKS_API_KEY environment variable to be set.

(require 'asdf)
(let ((asd (merge-pathnames "../litelm/litelm.asd"
                            (or *load-pathname* *default-pathname-defaults*))))
  (when (probe-file asd)
    (asdf:load-asd asd)))
(asdf:load-system :litelm)

(defpackage #:text-adventure
  (:use #:cl)
  (:export #:play))

(in-package #:text-adventure)

(defvar *fireworks-model* "fireworks-ai/accounts/fireworks/models/deepseek-v4-flash"
  "Fireworks model to play with, written as a litelm \"provider/model\" string.")

(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")

(defun chat (messages &key (model *fireworks-model*))
  "Send the multi-turn MESSAGES (a list of (role content) pairs) to Fireworks
   through litelm and return the assistant's text."
  (litelm:response-content
   (litelm:completion model :messages messages)))

(defun load-story (filepath)
  (handler-case
      (with-open-file (f filepath :direction :input)
        (let ((content (make-string (file-length f))))
          (read-sequence content f)
          content))
    (file-error (e)
      (declare (ignore e))
      (format t "Error: ~a not found.~%" filepath)
      nil)))

(defun play (&key (story-file *story-file*) (model *fireworks-model*))
  "Start the text adventure game. Reads story-file as the initial prompt and
   uses Fireworks AI, through litelm, to generate responses to player actions."
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list (list :system story))))
      (format t "~a~%~%" story)
      (format t "Welcome to the Text Adventure!~%")
      (format t "Describe what you want to do, or type 'quit' to exit.~%~%")
      (loop
        (format t "> ")
        (force-output)
        (let ((user-input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
          (when (member user-input '("quit" "exit") :test #'string-equal)
            (format t "Goodbye!~%")
            (return))
          ;; An empty line simply re-prompts: the body of the turn is skipped
          ;; and LOOP goes round again. (An earlier version tried to jump with
          ;; (go :continue), but LOOP defines no such tag, so pressing Enter
          ;; signalled "attempt to GO to nonexistent tag".)
          (unless (string= user-input "")
            (setf messages (append messages (list (list :user user-input))))
            (let ((response (chat messages :model model)))
              (when response
                (format t "~a~%" response)
                (setf messages (append messages
                                       (list (list :assistant response))))))))))))
