;;;; text-adventure-game_ollama.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;; Model access goes through the litelm routing library (../litelm), so this
;;;; file holds no HTTP or JSON code of its own.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game_ollama.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game_ollama.lisp --eval '(text-adventure:play)'
;;;;
;;;; Requires a local Ollama server with at least one chat model pulled.

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

(defvar *ollama-model* "qwen3.5:2b"
  "Ollama model to play with. litelm needs a \"provider/model\" string, so
   this may be either \"qwen3.5:2b\" or \"ollama/qwen3.5:2b\".")

(defvar *ollama-api-base* nil
  "Optional override for the Ollama base URL passed to litelm. NIL uses
   litelm's built-in default, http://localhost:11434/v1.")

(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")

(defun ensure-model-name (model)
  "Prefix MODEL with \"ollama/\" unless it already names a provider."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))

(defun chat (messages &key (model *ollama-model*))
  "Send the multi-turn MESSAGES (a list of (role content) pairs) to the local
   Ollama server through litelm and return the assistant's text."
  (litelm:response-content
   (litelm:completion (ensure-model-name model)
                      :messages messages
                      :api-base *ollama-api-base*)))

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

(defun play (&key (story-file *story-file*) (model *ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt and
   uses the local Ollama model, through litelm, to generate responses."
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
