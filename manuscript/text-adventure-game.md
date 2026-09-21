# AI-Powered Text Adventure Game

Interactive fiction, text adventure games where the player types commands and the computer describes what happens next, has been a beloved genre since the days of *Zork* and *Adventure*. Traditionally, these games were built by hand: every room, item, puzzle, and narrative branch had to be authored in advance. A game master (human or algorithmic) could only respond in ways the programmer anticipated.

Large language models change this entirely. Instead of scripting every possible scenario, we can give an LLM a *system prompt* that establishes the world, the rules, and the tone, then lets the model improvise. The player types whatever they want, and the LLM generates a coherent, creative response that respects everything that happened before. There are no hardcoded branches, no "I don't understand that" dead ends. The story emerges from the conversation.

This chapter builds a complete AI text adventure game in Common Lisp. We provide two backends: **Ollama** for running an LLM locally (no API keys, no usage costs) and **Fireworks.ai** for cloud-hosted inference that runs dramatically faster. Both variants share the same conversational architecture and the same `chat` function — only the model name changes — yet the program delivers an open-ended interactive storytelling experience.

## Architecture

The game follows a simple conversation loop:

1. **System prompt**: a text file (`story.txt`) that defines the game world and instructs the LLM to act as a game master.
2. **Message history**: an accumulating list of role/content pairs representing the full conversation.
3. **Player input**: free-form text typed at a prompt.
4. **LLM call**: the full message history is sent to Ollama's chat API; the assistant's reply is displayed.
5. **Append and repeat**: the reply is appended to the history so the LLM remembers past events.

The Ollama integration is handled by a small `chat` function defined in the game file itself, which hands the multi-turn conversation to `litelm:completion` and returns the model's reply. The one dependency is the **litelm** routing library (in `../litelm`), which owns all of the HTTP and JSON work.

## The System Prompt

Before looking at the code, here is the data that defines the game world — the contents of `story.txt`, sent as the initial system message (its lines are broken here to fit the page):

```text
You are a text adventure game master. Create an immersive, interactive story
for the player. Follow these rules:

1. Describe the current scene vividly but concisely - include what the player
   sees, hears, and smells.
2. Present 2-4 clear options for what the player can do next at the end of
   each response.
3. Track the player's inventory, health, and progress through the adventure.
4. Introduce surprising twists, interesting NPCs, and challenging obstacles.
5. Respond to the player's chosen action by advancing the story in a coherent way.
6. Maintain internal consistency - remember what has happened before.

The adventure begins in the Valley of the Troll. The player is a brave
adventurer seeking the Golden Chalice, rumored to be hidden deep within
the Troll's mountain lair. The Valley is misty and foreboding, with ancient
trees casting long shadows. A narrow path winds toward the mountain, while a
dark stream gurgles nearby. The Troll is known to collect valuable artifacts
but is also said to be open to clever negotiation.

Begin by describing the opening scene and presenting options for the player.
```

This prompt does several things at once. It establishes the LLM's *role* (game master), the *rules* of engagement (vivid descriptions, presented options, tracked state), the *setting* (Valley of the Troll), the *goal* (find the Golden Chalice), and a *hint* about gameplay style (negotiation is possible). A well-written system prompt is the difference between a generic chatbot and an immersive game. You can swap out `story.txt` to create entirely different adventures such as a space station mystery, a noir detective story, a fantasy quest - and this is all without changing a single line of code.

## The Game Code

The complete program lives in a single file. We load the **litelm** routing library, define our package, and implement a small `chat` function plus two game functions: `load-story` for reading the system prompt file, and `play` for the main game loop. There is no HTTP or JSON code in the game itself: `litelm:completion` builds the request, posts it to Ollama, and hands back the decoded reply.

Here is the full listing of `text-adventure-game_ollama.lisp`:

```lisp
;;;; text-adventure-game_ollama.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;; Model access goes through the litelm routing library (../litelm), so this
;;;; file holds no HTTP or JSON code of its own.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game_ollama.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game_ollama.lisp --eval '(text-adventure:play)'
;;;;
;;;; Requires a local Ollama server with at least one chat model pulled.

(require 'asdf)
(let ((asd (merge-pathnames "../litelm/litelm.asd"
                            (or *load-pathname* *default-pathname-defaults*))))
  (when (probe-file asd)
    (asdf:load-asd asd)))
(asdf:load-system :litelm)

(defpackage #:text-adventure
  (:use #:cl)
  (:export #:play))

(in-package #:text-adventure)

(defvar *ollama-model* "qwen3.5:2b"
  "Ollama model to play with. litelm needs a \"provider/model\" string, so
   this may be either \"qwen3.5:2b\" or \"ollama/qwen3.5:2b\".")

(defvar *ollama-api-base* nil
  "Optional override for the Ollama base URL passed to litelm. NIL uses
   litelm's built-in default, http://localhost:11434/v1.")

(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")

(defun ensure-model-name (model)
  "Prefix MODEL with \"ollama/\" unless it already names a provider."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))

(defun chat (messages &key (model *ollama-model*))
  "Send the multi-turn MESSAGES (a list of (role content) pairs) to the local
   Ollama server through litelm and return the assistant's text."
  (litelm:response-content
   (litelm:completion (ensure-model-name model)
                      :messages messages
                      :api-base *ollama-api-base*)))

(defun load-story (filepath)
  (handler-case
      (with-open-file (f filepath :direction :input)
        (let ((content (make-string (file-length f))))
          (read-sequence content f)
          content))
    (file-error (e)
      (declare (ignore e))
      (format t "Error: ~a not found.~%" filepath)
      nil)))

(defun play (&key (story-file *story-file*) (model *ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt and
   uses the local Ollama model, through litelm, to generate responses."
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list (list :system story))))
      (format t "~a~%~%" story)
      (format t "Welcome to the Text Adventure!~%")
      (format t "Describe what you want to do, or type 'quit' to exit.~%~%")
      (loop
        (format t "> ")
        (force-output)
        (let ((user-input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
          (when (member user-input '("quit" "exit") :test #'string-equal)
            (format t "Goodbye!~%")
            (return))
          ;; An empty line simply re-prompts: the body of the turn is skipped
          ;; and LOOP goes round again. (An earlier version tried to jump with
          ;; (go :continue), but LOOP defines no such tag, so pressing Enter
          ;; signalled "attempt to GO to nonexistent tag".)
          (unless (string= user-input "")
            (setf messages (append messages (list (list :user user-input))))
            (let ((response (chat messages :model model)))
              (when response
                (format t "~a~%" response)
                (setf messages (append messages
                                       (list (list :assistant response))))))))))))

```

### Loading the System

```lisp
(require 'asdf)
(let ((asd (merge-pathnames "../litelm/litelm.asd"
                            (or *load-pathname* *default-pathname-defaults*))))
  (when (probe-file asd)
    (asdf:load-asd asd)))
(asdf:load-system :litelm)
```

Because the example is meant to be loaded straight from the repository with `load`, it registers the sibling `../litelm` system itself before loading it. In a project of your own you would instead declare `:depends-on (#:litelm)` in your `.asd` file and let ASDF resolve it.

### Reading the Story File

```lisp
(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")
```

**\*story-file\*** anchors the default prompt file to the source file, so the game can be started from any directory — the earlier `"story.txt"` relative path only worked if you happened to launch Lisp from this directory.

```lisp
(defun load-story (filepath)
  (handler-case
      (with-open-file (f filepath :direction :input)
        (let ((content (make-string (file-length f))))
          (read-sequence content f)
          content))
    (file-error (e)
      (declare (ignore e))
      (format t "Error: ~a not found.~%" filepath)
      nil)))
```

**load-story** reads the whole file into a string: it allocates a string exactly the size of the file using `file-length`, fills it with `read-sequence`, and wraps the operation in `handler-case` so that a missing file produces a clean error message instead of dropping into the debugger.

### The Main Game Loop

The `play` function is the heart of the program. Let's walk through it section by section.

```lisp
(defun play (&key (story-file *story-file*) (model *ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt and
   uses the local Ollama model, through litelm, to generate responses."
  (let ((story (load-story story-file)))
```

The function accepts two keyword parameters. `story-file` defaults to `story.txt` beside the source file, and `model` defaults to **\*ollama-model\*** but can be overridden to use any model you have pulled locally.

```lisp
    (unless story
      (return-from play))
```

We load the story file. If it is missing (`load-story` returns `nil`), we exit early. The `unless`/`return-from` pattern is cleaner than nesting the entire game inside an `if`.

```lisp
    (let ((messages (list (list :system story))))
```

This is the critical data structure: the message history, a list of `(role content)` pairs in litelm's message format. The system message comes first and sets up the game world; the `:system`, `:user`, and `:assistant` keywords are translated by litelm into the `role` values the API expects, so nothing in the game has to know any JSON.

```lisp
      (format t "~a~%~%" story)
      (format t "Welcome to the Text Adventure!~%")
      (format t "Describe what you want to do, or type 'quit' to exit.~%~%")
```

We print the system prompt so the player sees the world description, followed by a welcome message and instructions.

```lisp
      (loop
        (format t "> ")
        (force-output)
        (let ((user-input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
```

The game loop is an infinite `loop` form. We print a `>` prompt, call `force-output` to ensure it appears before the blocking `read-line`, then read and trim the player's input.

```lisp
          (when (member user-input '("quit" "exit") :test #'string-equal)
            (format t "Goodbye!~%")
            (return))
```

Typing `quit` or `exit` ends the game. `string-equal` makes the test case-insensitive, so `Quit` and `QUIT` work too — the older `member` test against four literal spellings quietly missed those.

```lisp
          ;; An empty line simply re-prompts: the body of the turn is skipped
          ;; and LOOP goes round again. (An earlier version tried to jump with
          ;; (go :continue), but LOOP defines no such tag, so pressing Enter
          ;; signalled "attempt to GO to nonexistent tag".)
          (unless (string= user-input "")
```

An empty line is simply ignored: the `unless` skips the rest of the turn — no message is appended and the model is not called — and the loop prints the prompt again. The comment in the code is worth reading, because the obvious way to write this is wrong. An earlier draft used `(go :continue)`, assuming that `loop` provides a `:continue` tag. It does not: `loop` does expand into a `block` and a `tagbody`, but the tags it generates are its own, so `(go :continue)` is an error — *attempt to GO to nonexistent tag: :CONTINUE* — and the game died the first time the player pressed Enter on an empty line.

```lisp
            (setf messages (append messages (list (list :user user-input))))
```

We append the player's input as a user message. Note that we use `append` rather than a destructive operation: each turn creates a fresh list, which avoids mutation bugs. For a game that runs for dozens of turns the copying overhead is negligible.

```lisp
            (let ((response (chat messages :model model)))
              (when response
                (format t "~a~%" response)
                (setf messages (append messages
                                       (list (list :assistant response))))))))))))
```

We call `chat` with the full message history and the model name. `chat` hands the conversation to `litelm:completion`, which posts it to Ollama and returns the reply; we print it and append it to the history as an assistant message, so the model remembers what it said on future turns.

## The Ollama Chat Function

For completeness, here is everything the game does to talk to Ollama:

```lisp
(defvar *ollama-model* "qwen3.5:2b"
  "Ollama model to play with. litelm needs a \"provider/model\" string, so
   this may be either \"qwen3.5:2b\" or \"ollama/qwen3.5:2b\".")

(defvar *ollama-api-base* nil
  "Optional override for the Ollama base URL passed to litelm. NIL uses
   litelm's built-in default, http://localhost:11434/v1.")

(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")

(defun ensure-model-name (model)
  "Prefix MODEL with \"ollama/\" unless it already names a provider."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))

(defun chat (messages &key (model *ollama-model*))
  "Send the multi-turn MESSAGES (a list of (role content) pairs) to the local
   Ollama server through litelm and return the assistant's text."
  (litelm:response-content
   (litelm:completion (ensure-model-name model)
                      :messages messages
                      :api-base *ollama-api-base*)))
```

That is the entire transport layer. litelm addresses models as `"provider/model"` strings, so **ensure-model-name** prefixes a bare Ollama tag with `ollama/` — which also means a fully qualified `"ollama/qwen3.5:4b"` passes through untouched. Everything else belongs to litelm: building the JSON payload, POSTing it to `http://localhost:11434/v1/chat/completions`, decoding the reply, and mapping an HTTP failure onto a Lisp condition. **\*ollama-api-base\*** is there if you need to point at a different host; `nil` means litelm's default.

This is a large simplification over the earlier version of this file, which encoded the request with `cl-json`, patched the result with a string substitution (because `cl-json` writes `nil` as `null` where Ollama wants `false`), and shelled out to `curl` with `uiop:launch-program`. Routing through litelm removes all of it — and has the pleasant side effect that the same `chat` function serves the Fireworks variant, where only the model name changes.

### The Fireworks.ai variant

`text-adventure-game_fireworks.lisp` is the same program pointed at a cloud provider:

```lisp
(defvar *fireworks-model* "fireworks-ai/accounts/fireworks/models/deepseek-v4-flash"
  "Fireworks model to play with, written as a litelm \"provider/model\" string.")

(defparameter *story-file*
  (merge-pathnames "story.txt"
                   (make-pathname :name nil :type nil
                                  :defaults (or *load-truename*
                                                *default-pathname-defaults*)))
  "Default system-prompt file: story.txt next to this source file, so the game
   runs no matter what the REPL's current directory is.")

(defun chat (messages &key (model *fireworks-model*))
  "Send the multi-turn MESSAGES (a list of (role content) pairs) to Fireworks
   through litelm and return the assistant's text."
  (litelm:response-content
   (litelm:completion model :messages messages)))
```

The differences are only the model string — which already carries its `fireworks-ai/` provider prefix, so `ensure-model-name` is not needed — and the `FIREWORKS_API_KEY` environment variable that litelm reads for that provider. Everything else, including `play` and the game loop, is identical, which is the real payoff of routing through a shared library.

## Running the Game

Make sure Ollama is running and you have at least one chat model pulled:

```bash
ollama pull qwen3.5:2b
```

Then start your Lisp implementation and load the game:

```text
CL-USER 1 > (load "text-adventure-game_ollama.lisp")
CL-USER 2 > (text-adventure:play)
```

To use a different model, pass the `:model` keyword: `(text-adventure:play :model "qwen3.5:4b")`.

## Example Session

Here is a representative playthrough:

```text
CL-USER 1 > (load "text-adventure-game_ollama.lisp")
CL-USER 2 > (text-adventure:play)
You are a text adventure game master. Create an immersive, interactive story
for the player. Follow these rules:

1. Describe the current scene vividly but concisely - include what the player
   sees, hears, and smells.
2. Present 2-4 clear options for what the player can do next at the end of
   each response.
3. Track the player's inventory, health, and progress through the adventure.
4. Introduce surprising twists, interesting NPCs, and challenging obstacles.
5. Respond to the player's chosen action by advancing the story in a coherent
   way.
6. Maintain internal consistency - remember what has happened before.

The adventure begins in the Valley of the Troll...

Welcome to the Text Adventure!
Describe what you want to do, or type 'quit' to exit.

> I walk up the winding path, crossing the stream. What do I see?

The air hangs thick and damp over your shoulders as a low wind sweeps through
the valley floor, carrying the scent of pine resin and wet stone from across
the stream bed just ahead. Your boots kick up small ribbons of mud upon the
soft moss, which crunches softly underfoot with every step you take toward
the winding path.

Ahead lies the narrow trail leading to where the Troll's mountain looms in
silhouette above a dark cave mouth or cliff face that blocks off direct view
into the lair itself - the entrance hidden by dense trees and ancient stone
formations. The mist clings to your boots, dampening any potential light you
might hold against the gloom of the tunnel.

**Inventory:** {None} | **Health:** 10/10 (Freshly restored)

**Your Options:**

A) Follow the path upward and investigate the cave entrance directly for
   clues about how to reach or secure the Chalice, but beware of hidden traps.
B) Turn back toward the source of the valley mist; perhaps an elder troll
   guard waits there who knows where you came from?
C) Scan the surrounding area with your senses before proceeding further up
   towards higher ground for better visibility.

> I choose option C - I want to scan the area carefully before moving on.

You kneel down and run your fingers through the damp soil, closing your eyes
to focus on the subtle clues the valley might offer. The wind shifts...

> quit
Goodbye!
```

## What Makes This Work

The key insight is that the LLM is doing all the creative work (e.g., world-building, character dialogue, plot twists) while our Common Lisp code handles only the mechanical concerns: reading input, maintaining state, and routing messages. This separation of concerns is what makes the program so short.

**The message history is the game state.** There is no separate inventory tracker, health counter, or quest log in our code. The LLM tracks these in the conversation itself, for example notice how the assistant response includes "Inventory: {None} | Health: 10/10." This works because each API call includes the full conversation history, so the model can reference anything it (or the player) said previously.

**The system prompt does the heavy lifting.** By telling the model *how* to be a game master — describing scenes, presenting options, tracking state, maintaining consistency — we get structured, game-like responses without any parsing or post-processing. A poorly written system prompt would produce rambling narration without clear choices; a well-written one feels like a real game.

**Ollama makes it free and private.** Running locally means no API costs, no rate limits, and no data leaving your machine. The trade-off is that smaller models (2B–7B parameters) produce simpler stories than cloud-hosted giants like GPT-4 or Claude. For a fun interactive experience, the smaller models work surprisingly well.

## Customizing the Adventure

To create your own adventure, edit `story.txt` and change:

- **Setting**: Replace "Valley of the Troll" with your world such as a derelict spaceship, a 1920s speakeasy, or a wizard's tower.
- **Goal**: What is the player trying to accomplish? Find an artifact, solve a murder, escape a dungeon?
- **Rules**: Add constraints like "The player cannot use violence" or "All puzzles require rhyming solutions."
- **Tone**: Instruct the model to be humorous, terrifying, mysterious, or whimsical.

You can also experiment with different Ollama models. A larger model such as `qwen3.5:4b` produces richer prose and follows complex instructions better, while the default `qwen3.5:2b` is faster but may occasionally forget details from earlier in the conversation.

## Wrap Up

This chapter demonstrated how a small amount of Common Lisp code can harness an LLM to create an open-ended interactive experience. The pattern of using a system prompt, accumulating message history, calling chat API, and appending response generalizes beyond games to any conversational AI application: tutoring systems, interactive fiction, role-playing scenarios, and creative writing tools.

The complete source code lives in the `text-adventure-game` directory of the book's repository. Two backend variants are provided:

- `text-adventure-game_ollama.lisp`: uses Ollama for local, private, cost-free inference. Requires Ollama running locally with at least one chat model pulled.
- `text-adventure-game_fireworks.lisp`: uses the Fireworks.ai cloud API for much faster responses. Requires a `FIREWORKS_API_KEY` environment variable. Fireworks.ai hosts optimized versions of models like DeepSeek and Llama on dedicated GPU infrastructure, delivering sub-second latency that makes the game feel significantly more responsive.

Both files are under 100 lines of Lisp — most of it the game loop — because the depth comes from the LLM: it improvises within the constraints set by your system prompt.

## Optional Practice Problems

1. **Custom Adventure:** Create a new `story.txt` file with a completely different setting (space station, underwater city, haunted mansion). Run the game with your custom prompt and play through at least five turns. Observe how the model adapts to the new world. Compare the quality of responses with at least two different Ollama models.

2. **Save and Load:** Extend the game with a `save-game` function that writes the message history to a file (using `litelm:json-encode`) and a `load-game` function that restores it. Add commands `save` and `load` to the game loop. This reinforces working with JSON serialization and file I/O in Common Lisp.

3. **Model Switcher:** Add a command `model <name>` that changes the model mid-game without losing the conversation history. Implement this by modifying the `play` function to accept a dynamic model parameter rather than a one-time keyword argument. Consider: does switching models mid-story produce coherent results?

4. **Token Counter:** Add a diagnostic command `tokens` that estimates how many tokens the current message history contains. Ollama reports `prompt_eval_count` and `eval_count` on every response (visible in `litelm:response-raw`), so you can read the real numbers instead of estimating. If you would rather not depend on them, fall back to a heuristic: count characters and divide by 4. Print the estimate alongside the number of turns played.

5. **Multi-NPC Conversations:** Modify the system prompt to introduce two distinct NPCs with different personalities (e.g., a sarcastic goblin and a nervous elf). During play, try addressing each NPC by name and observe whether the model maintains distinct voices for each. Write a brief analysis of how well the model handles this and what prompt engineering techniques improved the results.

6. **Inventory System:** Enhance the `play` function to parse inventory changes from the LLM's responses. Look for patterns like "**Inventory:** {Sword, Shield}" in the response text, extract the items, and maintain a separate inventory list in Lisp. Display this list alongside the prompt. This exercise combines string parsing with state management.
