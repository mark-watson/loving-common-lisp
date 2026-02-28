# Common Lisp library to access Hugging Face Deep Learning APIs

From my book URI: https://leanpub.com/lovinglisp

Edit your file ~/.sbclrc to add the following line of code:

(pushnew #p"/Users/mark/GITHUB/loving-common-lisp/"
         ql:*local-project-directories*
         :test #'equal)

NOTE: Please change the path #p"/Users/mark/GITHUB/loving-common-lisp/" to the path where you cloned this repository using:

    git clone https://github.com/mark-watson/loving-common-lisp.git


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

## API documentation

- `huggingface:summarize` takes an input string and a maximum token count, calls Hugging Face's `facebook/bart-large-cnn` model, and returns the summary text extracted from the JSON response. The function requires the `HF_API_TOKEN` environment variable to be set so the generated `curl` request can authenticate.
- `huggingface:answer-question` accepts a question and supporting context, invokes the `deepset/roberta-base-squad2` question-answering model, and returns the highest-confidence answer string from the API response.
- `huggingface::huggingface-helper` is an internal utility that executes an arbitrary `curl` command via `uiop:run-program` and decodes the JSON response into a Common Lisp data structure; both public entry points rely on this helper.
