# Common Lisp library to access Tavily Web Search APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.

## setting your TAVILY_API_KEY API key
 
 Define the  "TAVILY_API_KEY" environment variable with the value of your Perplexity API key

## Examples:

```lisp
(tavily:websearch "Fun things to do in Sedona Arizona")
```
