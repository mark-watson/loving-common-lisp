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

## Example Run

```
CL-USER 1 > (load "text-adventure-game.lisp")
CL-USER 2 > (text-adventure:play)
You are a text adventure game master. Create an immersive, interactive story for the player. Follow these rules:

1. Describe the current scene vividly but concisely - include what the player sees, hears, and smells.
2. Present 2-4 clear options for what the player can do next at the end of each response.
3. Track the player's inventory, health, and progress through the adventure.
4. Introduce surprising twists, interesting NPCs, and challenging obstacles.
5. Respond to the player's chosen action by advancing the story in a coherent way.
6. Maintain internal consistency - remember what has happened before.

The adventure begins in the Valley of the Troll. The player is a brave adventurer seeking the Golden Chalice, rumored to be hidden deep within the Troll's mountain lair. The Valley is misty and foreboding, with ancient trees casting long shadows. A narrow path winds toward the mountain, while a dark stream gurgles nearby. The Troll is known to collect valuable artifacts but is also said to be open to clever negotiation.

Begin by describing the opening scene and presenting options for the player.


Welcome to the Text Adventure!
Describe what you want to do, or type 'quit' to exit.

> I walk up the winding path, crossing the stream. What do I see?
The air hangs thick and damp over your shoulders as a low wind sweeps through the valley floor, carrying the scent of pine resin and wet stone from across the stream bed just ahead. Your boots kick up small ribbons of mud upon the soft moss, which crunches softly underfoot with every step you take toward the winding path.

Ahead lies the narrow trail leading to where the Troll's mountain looms in silhouette above a dark cave mouth or cliff face that blocks off direct view into the lair itself—the entrance hidden by dense trees and ancient stone formations. The mist clings to your boots, dampening any potential light you might hold against the gloom of the tunnel.

**Inventory:** {None} | **Health:** 10/10 (Freshly restored)
*You are currently in the Valley floor near where a stream crossed under heavy rain clouds.*

**Your Options:**

A) Follow the path upward and investigate the cave entrance directly for clues about how to reach or secure the Chalice, but beware of hidden traps. B) Turn back toward the source of the valley mist; perhaps an elder troll guard waits there who knows where you came from?
C) Scan the surrounding area with your senses (smell wind patterns) before proceeding further up towards higher ground for better visibility.

*Which do you choose?*
> 
```
