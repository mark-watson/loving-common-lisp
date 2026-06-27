# Text Adventure Game in Common Lisp

An AI-driven text adventure game that uses Ollama for interactive storytelling. You describe what you want to do, and the LLM generates the story in response.

## Prerequisites

- [Ollama](https://ollama.com) running locally with at least one chat model pulled
- LispWorks (or SBCL with UIOP and cl-json via Quicklisp)
- [cl-json](https://github.com/sharplispers/cl-json) — loadable via Quicklisp: `(ql:quickload :cl-json)`
- `curl` available on your PATH

## Quick Start

1. Make sure Ollama is running and you have a model pulled:
   ```
   ollama pull mistral
   ```

2. Start LispWorks, then:
   ```lisp
   (load "text-adventure-game.lisp")
   (text-adventure:play)
   ```

3. To use a different model:
   ```lisp
   (text-adventure:play :model "qwen3.5:2b")
   ```

4. Type your actions at the `>` prompt. Type `quit` or `exit` to stop.

## Files

| File | Purpose |
|------|---------|
| `text-adventure-game.lisp` | Main game code — defines the `text-adventure` package with Ollama chat API integration |
| `story.txt` | System prompt that sets the adventure's world, tone, and rules |

## How It Works

The game maintains a growing message history:

1. **System prompt** — `story.txt` is sent as the initial system message, establishing the game world and the LLM's role as game master
2. **Player input** — each action you type is appended as a user message
3. **LLM response** — the full conversation history is sent to Ollama's `/api/chat` endpoint, and the assistant's reply is displayed
4. **Context grows** — every exchange is appended, so the AI remembers what happened earlier in the adventure

The Ollama API call is made via `curl`, with JSON encoding handled by `cl-json`.

## Customizing the Story

Edit `story.txt` to change the setting, characters, or rules. The system prompt tells the LLM what kind of game master to be. A well-written prompt includes:

- The setting and atmosphere
- The player's goal
- Any special rules or constraints
- Instructions for how to present choices

## Available Models

Choose any chat model you have pulled in Ollama. Smaller models (2B–4B params) are faster; larger models produce richer stories. Examples:

```
ollama pull mistral
ollama pull gemma4:12b-it-qat
ollama pull qwen3.5:2b
```
