;;;; nsk.asd --- System definition for the NSK engine.
;;;;
;;;; The core has no external dependencies so it loads under a bare LispWorks
;;;; image. The neural layer uses dexador when present (or a native socket),
;;;; and the server loads hunchentoot on demand, so neither is required here.

(asdf:defsystem "nsk"
  :description "NSK: Neural-Symbolic Knowledge Graph engine."
  :author "Mark Watson"
  :license "Apache-2.0"
  :version "1.0.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "json")
                             (:file "unify")
                             (:file "store")
                             (:file "reader")
                             (:file "neural")
                             (:file "query")
                             (:file "repl")
                             (:file "server")
                             (:file "main")))))
