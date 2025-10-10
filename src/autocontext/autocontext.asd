;; autocontext.asd
;;
;; To run this project:
;; 1. Make sure you have Python and the 'sentence-transformers' library installed:
;;    pip install sentence-transformers
;; 2. Place this file, bm25.lisp, main.lisp, and generate_embeddings.py
;;    in the same directory.
;; 3. Make sure that directory is in your ASDF source registry.
;; 4. Start your Lisp REPL and run:
;;    (ql:quickload :autocontext)
;;    (autocontext:run-example)

(defsystem #:autocontext
  :description "An intelligent context retriever in Common Lisp."
  :author "Gemini"
  :license "Public Domain"
  :depends-on (#:uiop #:split-sequence #:magicl #:yason)
  :components ((:file "bm25")
               (:file "main" :depends-on ("bm25"))))
