# MCP Common Lisp (`mcp-common-lisp`)

A minimal [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) server written in Common Lisp (SBCL) that communicates over `stdio` using JSON-RPC 2.0 and exposes macOS automation tools for Mail, Calendar, Reminders, and Messages.

## Prerequisites

- **macOS** (uses `osascript`, `sqlite3`, and macOS Mail, Calendar, Reminders, and Messages)
- **SBCL** (Steel Bank Common Lisp)
- **Quicklisp** (used to load `com.inuoe.jzon` and `uiop`)

## Project Structure

| File | Purpose |
| :--- | :--- |
| `mcp-common-lisp.asd` | ASDF system definition (`:mcp-common-lisp`, depends on `uiop` and `com.inuoe.jzon`) |
| `package.lisp` | Defines the `:mac` package shared by the macOS automation modules |
| `osascript.lisp` | AppleScript bridge (`run-osascript`, `osascript-error`, Mail/Calendar/Messages/Reminders helpers) |
| `read-recent-imessages.lisp` | Queries `~/Library/Messages/chat.db` via `sqlite3 -json` |
| `safari-login.lisp` | Standalone-capable Safari form-login helper (`mac:safari-login`, not exposed as an MCP tool) |
| `mcp-package.lisp` | Defines the `#:mcp-server` package with `jzon` local nickname |
| `mcp-server.lisp` | JSON-RPC 2.0 stdio transport, `deftool` macro, schema generation, and request dispatch |
| `mac-tools.lisp` | Registers the 5 macOS MCP tools using `deftool` |
| `run-server.sh` | Launches the MCP server on stdio using `sbcl` and `ql:quickload` |
| `test-session.sh` | Runs a non-destructive JSON-RPC test session against `run-server.sh` |
| `Makefile` | Convenience targets for testing, running, compiling, and building |

## Exposed MCP Tools

Tool names defined with `deftool` automatically convert Lisp hyphens to JSON underscores:

| MCP Tool Name | Parameters | Description |
| :--- | :--- | :--- |
| `mail_unread_count` | *(none)* | Return the number of unread messages in the macOS Mail inbox. |
| `todays_events` | *(none)* | List the titles of today's events from all calendars in macOS Calendar. |
| `add_reminder` | `task_name` (`string`, required) | Create a new reminder in the macOS Reminders app. |
| `read_recent_imessages` | `limit` (`integer`, default `5`, range `1..50`) | Read recent iMessages as JSON objects (`is_from_me`, `sender`, `text`). |
| `send_imessage` | `phone_number` (`string`, required), `message` (`string`, required) | Send an iMessage using the macOS Messages app. |

## Running and Testing

### Using `make`

```sh
# Run the non-destructive MCP stdio test session (does not touch macOS apps)
make test

# Start the MCP server on stdio
make run

# Load and compile the ASDF system without starting the server
make compile

# Build a standalone executable named ./mcp-server
make build

# Remove build artifacts
make clean
```

### Using the Shell Scripts Directly

Run the test session (sends `initialize`, `notifications/initialized`, `tools/list`, invalid-argument checks, `ping`, and malformed JSON):

```sh
./test-session.sh
```

Start the server directly on stdio (stdout carries only JSON-RPC responses; logs are written to stderr):

```sh
./run-server.sh
```

### Connecting an MCP Client

Configure your MCP client (such as Claude Desktop) to launch `run-server.sh` over `stdio`:

```json
{
  "mcpServers": {
    "common-lisp-macos": {
      "command": "/Users/markw/GITHUB/loving-common-lisp/src/MCP_common_lisp/run-server.sh"
    }
  }
}
```

Or build the standalone binary with `make build` and point `"command"` to the resulting `mcp-server` executable for faster startup.

### Using `:mac` Functions from a Lisp REPL

```lisp
(asdf:load-asd "mcp-common-lisp.asd")
(ql:quickload :mcp-common-lisp)

(mac:mail-unread-count)
(mac:get-todays-events)
(mac:add-reminder "Buy milk")
(mac:read-recent-imessages 5)
```

`safari-login.lisp` can also be loaded and called on its own (keep credentials in environment variables, never in source files):

```lisp
(load "package.lisp")
(load "safari-login.lisp")
(mac:safari-login "https://example.com/login"
                  (uiop:getenv "SITE_USER")
                  (uiop:getenv "SITE_PASSWORD"))
```

## macOS Permissions and Notes

- **Automation**: The first time the server controls Mail, Calendar, Reminders, Messages, or Safari, macOS will prompt you to grant Automation permission to your terminal/Lisp process under **System Settings > Privacy & Security > Automation**.
- **Full Disk Access**: `read_recent_imessages` reads `~/Library/Messages/chat.db`. You must grant **Full Disk Access** (**System Settings > Privacy & Security > Full Disk Access**) to the terminal or host application running the server.
- **Safari JavaScript**: `mac:safari-login` requires **Safari > Develop > Allow JavaScript from Apple Events** (or `defaults write -app Safari AllowJavaScriptFromAppleEvents 1`).
- **Protocol & Input Validation**: The server supports MCP protocol versions `2025-11-25`, `2025-06-18`, `2025-03-26`, and `2024-11-05`. Tool arguments are type-checked against their JSON Schema definitions before execution.
