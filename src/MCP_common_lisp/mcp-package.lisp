;;;; mcp-package.lisp -- package for the MCP server.

(defpackage #:mcp-server
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:deftool
           #:start
           #:handle-message
           #:*tools*))
