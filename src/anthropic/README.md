# Common Lisp library to access Anthropic LLM APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.


## setting your Anthropic API key
 
 Define the  "ANTHROPIC_API_KEY" environment variable with the value of your Anthropic API key
 
## Example:

```lisp
cl-user> (ql:quickload :anthropic)
cl-user> (anthropic:completions "The President went to Congress" 20)
"I don't have enough context to comment on a specific President going to Congress. As branches of the U.S. government, it's common for Presidents to engage with Congress on policy matters, budget proposals, nominations, and other issues within their constitutional roles and responsibilities."