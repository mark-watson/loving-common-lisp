;;;; mcp-common-lisp.asd -- an MCP server that exposes macOS automation tools.

(asdf:defsystem #:mcp-common-lisp
  :description "Model Context Protocol (MCP) server over stdio with macOS tools."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:com.inuoe.jzon)
  :serial t
  :components ((:file "package")
               (:file "osascript")
               (:file "read-recent-imessages")
               (:file "safari-login")
               (:file "mcp-package")
               (:file "mcp-server")
               (:file "mac-tools")))
