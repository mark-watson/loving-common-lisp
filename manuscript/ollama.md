# Local LLMs Using Ollama

*NOTE: as of December 27, 2025 this chapter is a work in progress.*

TBD

The **ollama** package developed here provides generative AI code and tool use/function calling generative AI code in the directory **loving-common-lisp/src/ollama**.

## Design Notes

TBD

## Implementation of Common Helper Code

TBD

Listing of package.lisp:

```lisp
;;;; package.lisp

(defpackage #:ollama
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:completions-with-tools #:summarize
           #:answer-question #:embeddings #:dot-product
           *model-name* *tool-model-name* *model-host*))
```

Listing of ollama.asd:

```lisp
;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "ollama-helper")
               (:file "ollama-tools") 
               (:file "ollama")))
```


