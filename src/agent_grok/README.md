# Agent System Using Grok API with Tool Calling

**Book Chapter:** [More Agents Using X's Grok and Perplexity APIs](https://leanpub.com/read/lovinglisp/more-agents-using-xs-grok-and-perplexity-apis) — *Loving Common Lisp* (free to read online).

> **Note:** This example is not yet included in the book.

This project demonstrates a Common Lisp agent system that uses the xAI Grok API with support for tool calling. The agent can be extended with custom tools via the `def-tool` macro. A second variant (`agent_grok_perplexity.lisp`) adds a web-search tool backed by the Perplexity Sonar API, giving the agent the ability to look up live information from the web.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- An xAI Grok API key — set the `X_GROK_API_KEY` environment variable
- (Optional) A Perplexity API key — set `PERPLEXITY_API_KEY` for web search support

## Dependencies

Loaded automatically via Quicklisp:
- `drakma`, `yason`, `alexandria`, `uiop`, `cl+ssl`

## Usage

```lisp
;; Load the base agent
(load "agent.lisp")

;; Run a query
(run-agent "What is the capital of France?")
```

To use the variant with Perplexity web search:

```lisp
(load "agent_grok_perplexity.lisp")
(run-agent "What happened in the news today?")
```

## How It Works

The agent sends a user query to the Grok API using an OpenAI-compatible chat completions endpoint. When the model decides it needs to call a tool, the agent dispatches the call locally, feeds the result back to the model, and returns the final answer. Custom tools are defined with `def-tool`, which registers a name, JSON schema, and a Lisp function.

## Files

| File | Description |
|------|-------------|
| `agent.lisp` | Core agent with tool-calling support via Grok |
| `agent_grok_perplexity.lisp` | Extended agent that adds a Perplexity web-search tool |
