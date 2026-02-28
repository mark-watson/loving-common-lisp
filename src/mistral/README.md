# Common Lisp library to access Mistral LLM APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.



## setting your mistral API key
 
 Define the  "MISTRAL_API_KEY" environment variable with the value of your mistral API key
 
## Example:

```lisp
cl-user> (ql:quickload :mistral)
cl-user> (mistral:completions "The President went to Congress" 200)
"When the President of a country goes to Congress, it typically means that they are making a formal address to a joint session of the legislative body. This is often done to present the State of the Union address, which outlines the administration's goals and priorities for the upcoming year. The President may also go to Congress to propose new legislation, rally support for existing bills, or address important national issues.

During the address, members of Congress from both parties are usually present in the chamber, and they may respond with applause, standing ovations, or other forms of expression. The President's speech is typically broadcast live on television and radio, and it is covered extensively by the news media.

The practice of the President going to Congress to deliver a State of the Union address dates back to the early years of the United States, when President George Washington gave the first such address in 1790. Since then, it has become a regular tradition for"
```

