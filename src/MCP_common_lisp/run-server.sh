#!/bin/sh
# Start the MCP server on stdio. Stdout carries only JSON-RPC messages.

DIR="$(cd "$(dirname "$0")" && pwd)"

exec sbcl --noinform --non-interactive \
  --eval "(asdf:load-asd \"$DIR/mcp-common-lisp.asd\")" \
  --eval "(ql:quickload :mcp-common-lisp :silent t)" \
  --eval "(mcp-server:start)"
