# Using the Perplexity Sonar Web Search and LLM APIs

As I write this chapter in late February 2025 I have spent a non-trivial amount of time in the last year integrating client code for using search APIs from Google, Bing, Brave, and Tavily with my own application logic using LLMs.

What I like about the [Perplexity Sonar Search and LLM APIs](https://sonar.perplexity.ai) is the integration of search and LLM processing that frequently satisfies my requirements without writing my own glue code. The example code for this chapter can be found at [https://github.com/mark-watson/perplexity](https://github.com/mark-watson/perplexity). As usual with my Common Lisp libraries please git clone the source code in your local Quicklisp directory

    ~/quicklisp/local-projects/

Here I use the more expensive model **sonar-pro** but if you need to perform bulk processing or many API requests please do your own experiments to see if the less expensive **sonar** model meets your requirements.

We will start by looking at an example using the short library developed later in this chapter.

## Example Library Use

The Sonar API is built on a model trained to use a web search tool (model function calling). Here we look at a difficult example: asking what musical instruments I play. This is difficult for two reasons:

- My name (Mark Watson) is mentioned on a huge number of web sites because there are a several famous people with my name (a comedian, an economist, etc.)
- On my own web properties I only mention instruments on one of my many web sites.

Let’s look at this example and discuss the output after this listing (here I am using LispWorks but this example works with SBCL and other Common Lisp implementations). I call **perplexity:research** with my query on line 5 and output is printed on lines 7-18:

```text
$ lw
; Loading text file /Applications/LispWorks 8.0 (64-bit)/Library/lib/8-0-0-0/private-patches/load.lisp
CL-USER 1 > (ql:quickload :perplexity)
CL-USER 2 > (perplexity:research "consultant Mark Watson has written AI and Lisp books. What musical instruments does he play?")
"Mark Watson, an AI practitioner and consultant, has written over 20 books on AI topics, including books on Lisp programming. According to his website, he plays the following musical instruments:

1. Guitar
2. Didgeridoo 
3. American Indian flute

His website mentions that in addition to programming and writing, his hobbies include playing these instruments. It also provides audio samples of his guitar and didgeridoo playing, as well as a recording of him playing American Indian flute with other musicians."
```

Other LLM web-based services like OpenAI’s web interface or App using ChatGPT 4o with web search and o3-mini-high with web search also correctly process this query (usually visiting about 15 web sites). The difference is that providers like OpenAI, Groq, etc. don’t offer search integrated with their LLM APIs; they just integrate search on their web and App interfaces as an option.

## Library Implementation

You need to register to get a Perplexity API key and set the environment variable **PERPLEXITY_API_KEY**.

```lisp
(in-package #:perplexity)

(defvar ;; we use Perplexity's OpenAI compatible API
  *model-host*
  "https://api.perplexity.ai/chat/completions")
(defvar *model* "sonar-pro")

(defun research-helper (curl-command)
  "this function is identical to the function openai-helper in
   the library https://github.com/mark-watson/openai"
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if Perplexity
        ;; or OpenAI changes JSON return format):
        (cdr
          (assoc
            :content
            (cdr (assoc :message (cadr (assoc :choices json-as-list))))))))))

(defun research (starter-text)
  "Send a search and LLM request"
  (let* ((input-text (write-to-string starter-text))
         (request-body
          (cl-json:encode-json-to-string
           `((:messages . (((:role . "user")
                            (:content . ,input-text))))
             (:model . ,*model*))))
         (curl-command
           (format nil 
              "curl ~A ~
              -H \"Content-Type: application/json\" ~
              -H \"Authorization: Bearer ~A\" ~
              -d '~A'"
              *model-host*
              (uiop:getenv "PERPLEXITY_API_KEY")
              request-body)))
    (research-helper curl-command)))
```

This Common Lisp code defines a function **research** that sends a user's query to Perplexity AI's API and returns the AI-generated response. This code is adapted from a similar library **openai** in the GitHub repository [github.com/mark-watson/openai](https://github.com/mark-watson/openai), but tailored here for Perplexity AI's OpenAI compatible API.

Currently the code in the GitHub repository for this library contains additional debug printouts.

## Wrap Up for the Perplexity Sonar API

The library developed here is short and simple but encapsulates a very common use case that I have: using LLMs grounded in realtime search results for reasoning and question answering. Here we used the **sonar-pro** model. Here is a list of available models with their costs:

- sonar-reasoning-pro $2 per million input tokens, $8 per million output tokens, and $5 per 1000 searches
- sonar-reasoning $1 per million input tokens, $5 per million output tokens, and $5 per 1000 searches
- sonar-pro $3 per million input tokens, $15 per million output tokens, and $5 per 1000 searches
- sonar $1 per million input tokens, $1 per million output tokens, and $5 per 1000 searches

In practice the least expensive **sonar** model usually works well for me but if your use of this API is light, as mine is, it makes sense to just use the best model.

## Wrap Up Part 2: Perplexity Offers a Derivative DeepSeek R1 Model on its APIs

DeepSeek R1 is an open-source AI language model developed by the Chinese company DeepSeek (subsidiary of the High-Flyer Chinese hedge fund), offering performance comparable to leading models like OpenAI's GPT-4 at a significantly lower cost. Perplexity tuned a distilled version of this model for use in the USA and makes it available as the model **r1-1776** for the following price:

- r1-1776 $2 per million input tokens and $8 for million output tokens.

The incredibly good DeepSeek R1 model is available from many hosts in the USA and Europe, often at a much less expensive price that Perplexity. As always, when building applications first get the required functionality and then research for the least expensive hosting options. For example:

- DeepSeek offers R1 (hosted in China) for $0.55 per million tokens, $2.19 per million output tokens.
- Groq (USA company that uses their own custom hardware for LLM inferencing) offers the DeepSeek R1 Distill Llama 70B model for $0.75 per million input tokens and $0.99 for million output tokens at the endpoint https://api.groq.com/openai/v1/chat/completions (OpenAI compatible).
