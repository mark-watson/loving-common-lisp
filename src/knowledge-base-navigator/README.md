# Knowledge Base Navigator (Gemini Edition)

**Book Chapter:** [Knowledge Base Navigator: Building an AI-Powered Information System Using LLMs](https://leanpub.com/read/lovinglisp/knowledge-base-navigator-building-an-ai-powered-information-system-using-llms) — *Loving Common Lisp* (free to read online).


Welcome to the **Knowledge Base Navigator**, an AI-powered text interface designed to extract, disambiguate, and retrieve encyclopedic data on entities from natural language input.

This project is a modern evolution of the legacy Knowledge Graph Navigator (KGN) system. It completely shifts the backend from NLP/DBPedia/SPARQL queries to a highly efficient LLM pipeline powered by the Gemini 3 Flash API.

## Program Design

The system mimics the classic Knowledge Graph Navigator workflow but replaces piecemeal database queries with a generative AI pipeline. The design follows a two-stage process:

1. **Entity Extraction & Disambiguation:** 
   The user provides a sentence or a list of words. The system sends this text to the Gemini API, instructing it to identify potential encyclopedic entities (people, companies, countries, cities, concepts) and return them as a numbered list with short descriptive summaries.
   
2. **Deep Retrieval & Relationship Mapping:** 
   The user types the numeric indices of the entities they wish to learn more about. The system sends a second prompt to Gemini, feeding it the user's specific selections. It asks for detailed, factual information on each entity (e.g., birthplaces, financial data, industries) and explicitly requests an analysis of the relationships and associations among all selected entities.

## Implementation Details

The implementation is written entirely in Common Lisp and comprises three primary files:

- `knowledge-base-navigator.asd`: The ASDF system definition file that tracks dependencies and compilation order.
- `project.lisp`: Manages the namespace definition (`defpackage #:knowledge-base-navigator`) and handles environment-level configurations.
- `knowledge-base-navigator.lisp`: The core application file containing the `kbn-ui` loop and the Gemini API utility `get-gemini-chat-completion`.

**API Communication (`curl` over HTTP Libraries):**  
To ensure maximum cross-platform reliability and avoid Common Lisp HTTP library dependency friction (e.g., SSL issues with `dexador` or `drakma`), the project invokes raw `curl` commands via `uiop:run-program`. To bypass shell escaping issues with complex JSON payloads, the Lisp code safely writes the JSON encoded by `cl-json` to a temporary file (`mktemp`) and passes it to `curl` using the `-d @filename` syntax.

## Requirements

Ensure you meet the following requirements before running the system:
* A modern Common Lisp implementation (designed/tested with SBCL).
* **Quicklisp** for dependency management.
* The system relies on the following packages:
  * `cl-json`
  * `uiop` (bundled with modern ASDF)
  * `alexandria`
* The `curl` command-line tool must be installed and available in your `$PATH`.

### API Key

You must have a valid Google Gemini API key exported in your environment variable prior to starting your Lisp process.

```bash
export GEMINI_API_KEY="your_actual_api_key_here"
```

## Usage Instructions

The easiest way to run the application is to load the ASDF system via Quicklisp. Depending on your configuration, ensure the directory holding `knowledge-base-navigator.asd` is accessible to Quicklisp (for example, symlinked inside `~/quicklisp/local-projects/`).

### Loading from REPL

Start your Lisp REPL (e.g., `sbcl`), load the system, and start the UI loop:

```lisp
;; Load the ASDF system
(ql:quickload "knowledge-base-navigator")

;; Start the interactive text UI
(knowledge-base-navigator:kbn-ui)
```

### Running an Interactive Session

When the `kbn-ui` loop starts, you will see a prompt. Type a sentence that includes entities:

```text
============= GEMINI KNOWLEDGE BASE NAVIGATOR =============

Enter entity names separated by space or a descriptive sentence (or type 'quit' to exit):
> Bill Gates and Microsoft

[Extracting entities using Gemini 3 Flash...]

--- IDENTIFIED ENTITIES ---
1. Bill Gates: An American business magnate, software developer, and philanthropist who co-founded Microsoft Corporation.
2. Microsoft: A multinational technology corporation that develops, manufactures, and licenses computer software, consumer electronics, and personal computers.
---------------------------

Enter the numbers of the entities you want detailed information for (space separated):
> 1 2

[Fetching detailed facts and relationships for selected entities...]
```

The system will retrieve detailed attributes about the distinct entities you chose, followed by a dynamically generated factual summary regarding how they relate to each other.
