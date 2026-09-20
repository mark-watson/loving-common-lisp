;; autocontext.asd
;;
;; To run this project:
;; 1. Install the Lisp dependencies from Quicklisp, including MAGICL
;;    (matrix algebra; see the chapter for what it is used for):
;;      (ql:quickload '(:magicl :yason :split-sequence))
;; 2. The Python embedding helper runs under uv, which reads pyproject.toml
;;    from this directory. Run it once from the command line to download the
;;    sentence-transformers model (a few minutes, a couple hundred MB):
;;      echo "some text" | uv run generate_embeddings.py
;; 3. Make sure this directory is in your ASDF source registry.
;; 4. Start your Lisp REPL and run:
;;      (ql:quickload :autocontext)
;;      (autocontext:run-example)

(defsystem #:autocontext
  :description "An intelligent context retriever in Common Lisp."
  :author "Gemini"
  :license "Public Domain"
  :depends-on (#:uiop #:split-sequence #:magicl #:yason #:litelm)
  :components ((:file "bm25")
               (:file "main" :depends-on ("bm25"))))
