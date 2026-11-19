#!/bin/sh
# Send a short MCP session to the server and print its replies.
# These calls do not touch any macOS app.

DIR="$(cd "$(dirname "$0")" && pwd)"

"$DIR/run-server.sh" 2>/dev/null <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"read_recent_imessages","arguments":{"limit":"5; DROP TABLE message"}}}
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"add_reminder","arguments":{}}}
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"no_such_tool","arguments":{}}}
{"jsonrpc":"2.0","id":6,"method":"ping"}
not json
EOF
