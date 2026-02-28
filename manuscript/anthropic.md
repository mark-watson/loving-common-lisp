# Using the Anthropic Claude LLM Completion API

The Common Lisp implementation for the Anthropic Claude APIs in this chapter is similar to what we saw in the last chapter accessing the OpenAI GPT APIs.

You will want to use the [Anthropic completion API dcumentation](https://docs.anthropic.com/claude/reference/complete_post) for reference.

## History of Anthropic and How They Differ From OpenAI

[Anthropic](https://en.wikipedia.org/wiki/Anthropic) is an AI startup and public benefit corporation founded by former members of OpenAI. The company was founded in 2021 by siblings Daniela Amodei and Dario Amodei, who were senior members of OpenAI. Dario Amodei served as OpenAI's Vice President of Research. They left OpenAI due to directional differences, specifically regarding OpenAI's ventures with Microsoft in 2019. As I write this in October 2023 Anthropic has just taken investment from Amazon and will use AWS infrastructure in the future.

Anthropic specializes in developing general AI systems and language models, with a company goal of supporting responsible AI usage. They have developed their own AI chatbot named Claude. Claude uses a messaging interface where users can submit questions or requests and receive highly detailed and relevant responses. We will not use the Claude chatbot here, instead we will use their text completion API. You will need to get an API key:

[https://docs.anthropic.com/claude/reference/getting-started-with-the-api](https://docs.anthropic.com/claude/reference/getting-started-with-the-api)

The differences between Anthropic and OpenAI are mainly in their focus areas and outputs. While both companies conduct cutting-edge machine learning research, Anthropic's research is mainly focused on generative models and how to align them with human values. On the other hand OpenAI has developed the popular ChatGPT generative AI platform that is now widely used.

Another key difference is their strategic partnerships. Anthropic has a partnership with Google and Amazon, while OpenAI has Microsoft's backing. These partnerships influence the direction of their research and the development of their products.

In terms of products Anthropic's Claude 2 goes head-to-head against ChatGPT. However, like OpenAI's models, Claude 2 still exhibits bias and sometimes hallucinates incorrect responses.


## Common Lisp Library for the Anthropic LLM Completion API

The following Common Lisp code is very similar to what we used in the last chapter to call OpenAI's APIs.

The GitHub repository for this Quicklisp compatible library can be found here:

[https://github.com/mark-watson/anthropic](https://github.com/mark-watson/anthropic)


```lisp
(in-package #:anthropic)

;; define the environment variable "ANTHROPIC_API_KEY" with
;; the value of your ANTHROPIC_API_KEY API key

(defvar anthropic-host "https://api.anthropic.com/v1/complete")

(defun anthropic-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        (string-trim
          " "
          (cdar json-as-list))))))

(defun completions (text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl --request POST --url " anthropic-host
           " -H \"accept: application/json\""
           " -H \"anthropic-version: 2023-06-01\""
           " -H \"content-type: application/json\""
           " -H \"x-api-key: " (uiop:getenv "ANTHROPIC_API_KEY") "\""
           " --data '{ \"prompt\": \"\\n\\nHuman: "
           text
           "\\n\\nAssistant: \", \"max_tokens_to_sample\": "
           (princ-to-string max-tokens)
           ", \"model\": \"claude-instant-1\" }'")))
    (anthropic-helper curl-command)))

```

Here is an example:

```lisp
cl-user> (ql:quickload :anthropic)
cl-user> (anthropic:completions "The President went to Congress" 200)
"I don't have enough context to comment on a specific President going to Congress. As branches of the U.S. government, it's common for Presidents to engage with Congress on policy matters, budget proposals, nominations, and other issues within their constitutional roles and responsibilities."
cl-user> (anthropic:completions "Answer concisely. Mary is 30 years old and Bob is 25. Who is older?" 12)
"Mary is older than Bob."
```

## Anthropic Text Completion API Wrapup

I have been using OpenAI APIs for about a year while I just received access to Anthropic's APIs last week so I am still experimenting running the same prompts against both OpenAI and Anthropic. Since there are no standards for interaction and calling LLM APIs there is a small overhead for switching between Anthropic and OpenAI APIs. There is at least one open source project that provides a uniform interface for OpenAI and Anthropic if portability is important to you.