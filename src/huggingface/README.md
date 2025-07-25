# Common Lisp library to access Hugging Face Deep Learning APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.


## setting your Hugging Face API key
 
 Define the  "HF_API_TOKEN" environment variable with the value of your Hugging Face API key

## Examples:

```lisp
cl-user> (ql:quickload :huggingface)
cl-user> (huggingface:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 30)
"Jupiter is the fifth planet from the Sun and the largest in the Solar System. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows. It is on average the third-brightest natural object in the night sky after the Moon and Venus. It has been known to ancient civilizations since before recorded history."

cl-user> (huggingface:answer-question "Where were the 1992 Olympics held?" "The 1992 Olympics were in Greece")
"Greece"
```

See my book for examples and example output.
