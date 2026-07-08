# AI-Powered Text Adventure Game

Interactive fiction — text adventure games where the player types commands and the computer describes what happens next — has been a beloved genre since the days of *Zork* and *Adventure*. Traditionally, these games were built by hand: every room, item, puzzle, and narrative branch had to be authored in advance. A game master (human or algorithmic) could only respond in ways the programmer anticipated.

Large language models change this entirely. Instead of scripting every possible scenario, we can give an LLM a *system prompt* that establishes the world, the rules, and the tone — then let the model improvise. The player types whatever they want, and the LLM generates a coherent, creative response that respects everything that happened before. There are no hardcoded branches, no "I don't understand that" dead ends. The story emerges from the conversation.

This chapter builds a complete AI text adventure game in Common Lisp. We provide two backends: **Ollama** for running an LLM locally (no API keys, no usage costs) and **Fireworks.ai** for cloud-hosted inference that runs dramatically faster. Both variants use the same conversational architecture; the program is under 60 lines of code, yet it delivers an open-ended interactive storytelling experience.

## Architecture

The game follows a simple conversation loop:

1. **System prompt** — a text file (`story.txt`) that defines the game world and instructs the LLM to act as a game master.
2. **Message history** — an accumulating list of role/content pairs representing the full conversation.
3. **Player input** — free-form text typed at a prompt.
4. **LLM call** — the full message history is sent to Ollama's chat API; the assistant's reply is displayed.
5. **Append and repeat** — the reply is appended to the history so the LLM remembers past events.

The Ollama integration is handled by the `ollama` package (developed in an earlier LLM chapter), which provides the `chat` function for multi-turn conversations. We load the `llm` ASDF system, which pulls in `ollama`, `cl-json`, `uiop`, and all other dependencies.

## The System Prompt

Before looking at the code, here is the data that defines the game world. This file is sent as the initial system message:

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

This prompt does several things at once. It establishes the LLM's *role* (game master), the *rules* of engagement (vivid descriptions, presented options, tracked state), the *setting* (Valley of the Troll), the *goal* (find the Golden Chalice), and a *hint* about gameplay style (negotiation is possible). A well-written system prompt is the difference between a generic chatbot and an immersive game. You can swap out `story.txt` to create entirely different adventures — a space station mystery, a noir detective story, a fantasy quest — without changing a single line of code.

## The Game Code

The complete program lives in a single file. We load the `llm` system, define our package, and implement two functions: `load-story` for reading the system prompt file, and `play` for the main game loop.

Here is the full listing of `text-adventure-game.lisp`:

```lisp
;;;; text-adventure-game.lisp
;;;; Text adventure game using Ollama for AI-driven storytelling.
;;;;
;;;; Usage (LispWorks):
;;;;   (load "text-adventure-game.lisp")
;;;;   (text-adventure:play)
;;;;
;;;; Usage (SBCL):
;;;;   sbcl --load text-adventure-game.lisp --eval '(text-adventure:play)'

(ql:quickload :llm)

(defpackage #:text-adventure
  (:use #:cl)
  (:export #:play))

(in-package #:text-adventure)

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

(defun play (&key (story-file "story.txt") (model ollama:*ollama-model*))
  "Start the text adventure game. Reads story-file as the initial prompt
   and uses Ollama to generate responses to player actions."
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
    (let ((messages (list (list (cons :|role| "system")
                                (cons :|content| story)))))
      (format t "~a~%~%" story)
      (format t "Welcome to the Text Adventure!~%")
      (format t "Describe what you want to do, or type 'quit' to exit.~%~%")
      (loop
        (format t "> ")
        (force-output)
        (let ((user-input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
          (when (member user-input '("quit" "exit" "QUIT" "EXIT") :test #'string=)
            (format t "Goodbye!~%")
            (return))
          (when (string= user-input "")
            (go :continue))
          (setf messages (append messages
                                 (list (list (cons :|role| "user")
                                             (cons :|content| user-input)))))
          (let ((response (ollama:chat messages :model-id model)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list (list (cons :|role| "assistant")
                                                 (cons :|content| response))))))))))))
```

### Loading the System

```lisp
(ql:quickload :llm)
```

This single line loads the `llm` ASDF system, which includes the `ollama` package (providing `chat`), `cl-json` (JSON encoding/decoding), `uiop` (subprocess management), and Dexador (HTTP client). All of the HTTP and JSON plumbing lives in the `ollama` package — our game code never touches it directly.

### Reading the Story File

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

`load-story` reads the entire contents of a file into a string. It allocates a string exactly the size of the file using `file-length`, then fills it with `read-sequence`. The `handler-case` wraps the operation so that a missing file produces a clean error message instead of dropping into the debugger.

### The Main Game Loop

The `play` function is the heart of the program. Let's walk through it section by section.

```lisp
(defun play (&key (story-file "story.txt") (model ollama:*ollama-model*))
```

The function accepts two keyword parameters. `story-file` defaults to `"story.txt"` in the current directory. `model` defaults to the Ollama model defined in the `ollama` package — you can override it to use any model you have pulled locally.

```lisp
  (let ((story (load-story story-file)))
    (unless story
      (return-from play))
```

We load the story file. If it is missing (`load-story` returns `nil`), we exit early. The `unless`/`return-from` pattern is cleaner than nesting the entire game inside an `if`.

```lisp
    (let ((messages (list (list (cons :|role| "system")
                                (cons :|content| story)))))
```

This is the critical data structure: the message history. It is a list of association lists, each with a `role` and `content` key. The system message comes first and sets up the game world. The escaped symbol syntax `:|role|` and `:|content|` ensures that `cl-json` encodes these keys in lowercase as `"role"` and `"content"`, which is what Ollama's API expects.

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
          (when (member user-input '("quit" "exit" "QUIT" "EXIT") :test #'string=)
            (format t "Goodbye!~%")
            (return))
          (when (string= user-input "")
            (go :continue))
```

Two early exits: typing "quit" or "exit" (case-insensitive) ends the game via `return`, and empty input restarts the loop via `go :continue`. The `:continue` tag is provided implicitly by the `loop` form.

```lisp
          (setf messages (append messages
                                 (list (list (cons :|role| "user")
                                             (cons :|content| user-input)))))
```

We append the player's input as a user message to the history. Note that we use `append` rather than a destructive operation — each turn creates a fresh list, which avoids mutation bugs. For a game that runs for dozens of turns the copying overhead is negligible.

```lisp
          (let ((response (ollama:chat messages :model-id model)))
            (when response
              (format t "~a~%" response)
              (setf messages (append messages
                                     (list (list (cons :|role| "assistant")
                                                 (cons :|content| response))))))))))))
```

We call `ollama:chat` with the full message history and the model ID. The `chat` function (defined in the `ollama` package) serializes the messages to JSON, sends them to Ollama's `/api/chat` endpoint via `curl`, and parses the assistant's response. We print the response and append it to the history as an assistant message, so the LLM remembers what it said on future turns.

## The Ollama Chat Function

For completeness, here is the `chat` function from the `ollama` package (defined in the directory **loving-common-lisp/src/llm**) that our game calls:

```lisp
(defun chat (messages &key (model-id *ollama-model*))
  (let* ((data (list (cons :|model| model-id)
                      (cons :|stream| nil)
                      (cons :|messages| messages)))
         (json-data (cl-json:encode-json-to-string data))
         (fixed-json-data
          (llm:substitute-subseq json-data ":null" ":false" :test #'string=))
         (process (uiop:launch-program
                   (format nil "curl -s ~a -d ~s" *ollama-endpoint* fixed-json-data)
                   :output :stream
                   :error-output :stream))
         (response (with-output-to-string (out)
                     (loop for line = (read-line (uiop:process-info-output process) nil nil)
                           while line
                           do (write-line line out)))))
    (with-input-from-string (s response)
      (let* ((json-as-list (cl-json:decode-json s))
             (message-resp (cdr (assoc :message json-as-list)))
             (content (cdr (assoc :content message-resp))))
        (or content "No response content")))))
```

The function builds a JSON payload with the model ID, `stream` set to `false`, and the message list. It uses `cl-json:encode-json-to-string` for serialization, then fixes a Common Lisp JSON encoding quirk: `cl-json` encodes `nil` as `null`, but Ollama expects `false` for the stream field. The `llm:substitute-subseq` helper performs a single string substitution. The request is sent via `curl` launched as a subprocess with `uiop:launch-program`, reading the response line by line from the process output stream.

## Running the Game

Make sure Ollama is running and you have at least one chat model pulled:

```bash
ollama pull qwen3.5:2b
```

Then start your Lisp implementation and load the game:

```lisp
CL-USER 1 > (load "text-adventure-game.lisp")
CL-USER 2 > (text-adventure:play)
```

To use a different model, pass the `:model` keyword:

```lisp
(text-adventure:play :model "mistral:v0.3")
```

## Example Session

Here is a representative playthrough:

```text
CL-USER 1 > (load "text-adventure-game.lisp")
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
into the lair itself — the entrance hidden by dense trees and ancient stone
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

> I choose option C — I want to scan the area carefully before moving on.

You kneel down and run your fingers through the damp soil, closing your eyes
to focus on the subtle clues the valley might offer. The wind shifts...

> quit
Goodbye!
```

## What Makes This Work

The key insight is that the LLM is doing all the creative work — world-building, character dialogue, plot twists — while our Common Lisp code handles only the mechanical concerns: reading input, maintaining state, and routing messages. This separation of concerns is what makes the program so short.

**The message history is the game state.** There is no separate inventory tracker, health counter, or quest log in our code. The LLM tracks these in the conversation itself — notice how the assistant response includes "Inventory: {None} | Health: 10/10." This works because each API call includes the full conversation history, so the model can reference anything it (or the player) said previously.

**The system prompt does the heavy lifting.** By telling the model *how* to be a game master — describe scenes, present options, track state, maintain consistency — we get structured, game-like responses without any parsing or post-processing. A poorly written system prompt would produce rambling narration without clear choices; a well-written one feels like a real game.

**Ollama makes it free and private.** Running locally means no API costs, no rate limits, and no data leaving your machine. The trade-off is that smaller models (2B–7B parameters) produce simpler stories than cloud-hosted giants like GPT-4 or Claude. For a fun interactive experience, the smaller models work surprisingly well.

## Customizing the Adventure

To create your own adventure, edit `story.txt` and change:

- **Setting**: Replace "Valley of the Troll" with your world — a derelict spaceship, a 1920s speakeasy, a wizard's tower.
- **Goal**: What is the player trying to accomplish? Find an artifact, solve a murder, escape a dungeon?
- **Rules**: Add constraints like "The player cannot use violence" or "All puzzles require rhyming solutions."
- **Tone**: Instruct the model to be humorous, terrifying, mysterious, or whimsical.

You can also experiment with different Ollama models. Larger models like `gemma3:12b` or `mistral:v0.3` produce richer prose and follow complex instructions better. Smaller models like `qwen3.5:2b` are faster but may occasionally forget details from earlier in the conversation.

## Wrap Up

This chapter demonstrated how a small amount of Common Lisp code can harness an LLM to create an open-ended interactive experience. The pattern — system prompt, accumulate message history, call chat API, append response — generalizes beyond games to any conversational AI application: tutoring systems, interactive fiction, role-playing scenarios, and creative writing tools.

The complete source code lives in the `text-adventure-game` directory of the book's repository. Two backend variants are provided:

- `text-adventure-game_ollama.lisp` — uses Ollama for local, private, cost-free inference. Requires Ollama running locally with at least one chat model pulled.
- `text-adventure-game_fireworks.lisp` — uses the Fireworks.ai cloud API for much faster responses. Requires a `FIREWORKS_API_KEY` environment variable. Fireworks.ai hosts optimized versions of models like DeepSeek and Llama on dedicated GPU infrastructure, delivering sub-second latency that makes the game feel significantly more responsive.

Both files are under 60 lines of Lisp; the real depth comes from the LLM's ability to improvise within the constraints set by your system prompt.

## Optional Practice Problems

1. **Custom Adventure:** Create a new `story.txt` file with a completely different setting (space station, underwater city, haunted mansion). Run the game with your custom prompt and play through at least five turns. Observe how the model adapts to the new world. Compare the quality of responses with at least two different Ollama models.

2. **Save and Load:** Extend the game with a `save-game` function that writes the message history to a file (using `cl-json:encode-json`) and a `load-game` function that restores it. Add commands `save` and `load` to the game loop. This reinforces working with JSON serialization and file I/O in Common Lisp.

3. **Model Switcher:** Add a command `model <name>` that changes the model mid-game without losing the conversation history. Implement this by modifying the `play` function to accept a dynamic model parameter rather than a one-time keyword argument. Consider: does switching models mid-story produce coherent results?

4. **Token Counter:** Add a diagnostic command `tokens` that estimates how many tokens the current message history contains. Since Ollama does not expose a token-counting endpoint directly, implement a simple heuristic: count characters and divide by 4 (a rough approximation for English text). Print the estimate alongside the number of turns played.

5. **Multi-NPC Conversations:** Modify the system prompt to introduce two distinct NPCs with different personalities (e.g., a sarcastic goblin and a nervous elf). During play, try addressing each NPC by name and observe whether the model maintains distinct voices for each. Write a brief analysis of how well the model handles this and what prompt engineering techniques improved the results.

6. **Inventory System:** Enhance the `play` function to parse inventory changes from the LLM's responses. Look for patterns like "**Inventory:** {Sword, Shield}" in the response text, extract the items, and maintain a separate inventory list in Lisp. Display this list alongside the prompt. This exercise combines string parsing with state management.
