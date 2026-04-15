# Notes for using AGENT.skill

## gemini-cli

### One time only: install skill

Symlink (Recommended for local development): If you want to keep the skill in your book's repository but use it globally, link it using the CLI:

```
gemini skills link cl-book-apis
```


You can verify it is installed by running:

```
gemini skills list
```

### Using the new skill

In gemini, try to use the skill:

```
/cl-book-apis-new write a Common Lisp fnction to get the proper nouns from a string of text using gemini APIs 
```

Output looks like;

```
 To extract proper nouns using the Gemini API, you can use the gemini:generate function from the gemini package (part of the project's
  library set). Below is a function that sends a targeted prompt to Gemini and parses the resulting comma-separated list into a Lisp
  list of strings.

    1 ;; Ensure the required packages are loaded
    2 (ql:quickload '(:gemini :myutils))
    3
    4 (defun get-proper-nouns-from-text (text)
    5   "Uses the Gemini API to extract proper nouns from the provided TEXT.
    6    Returns a list of strings representing the proper nouns."
    7   (let* ((prompt (format nil "Extract all proper nouns from the following text and return them ONLY as a comma-separated list. Do
      not include any introductory or concluding text: ~%~%~A" text))
    8          (response (gemini:generate prompt)))
    9     ;; Clean up the response and tokenize it by commas and newlines.
   10     ;; 'myutils:tokenize-string' handles splitting and trimming based on delimiters/discard lists.
   11     (myutils:tokenize-string response 
   12                              :delimiters '(#\, #\Newline #\Return) 
   13                              :discard '(#\Space #\Newline #\Return #\, #\.))))
   14
   15 ;; Example usage:
   16 ;; (get-proper-nouns-from-text "Steve Jobs and Steve Wozniak founded Apple in Cupertino, California.")
   17 ;; => ("Steve Jobs" "Steve Wozniak" "Apple" "Cupertino" "California")

  Key Components:
   1. Prompt Engineering: The prompt specifically asks for a "comma-separated list" and instructs Gemini to omit any extra
      conversational text, making it easy to parse.
   2. gemini:generate: This function (defined in gemini.lisp) handles the API call to Google's Generative Language API using curl.
   3. myutils:tokenize-string: This utility function is used to split the returned string into a list. By setting specific :delimiters
      (commas and newlines) and :discard characters (spaces and punctuation), it ensures the resulting list contains clean strings.

  Note: Ensure your GOOGLE_API_KEY environment variable is set before calling this function, as the gemini package depends on it.
```


## Google Antigravity

I each project directory:

Place the **loving-common-lisp/src/cl-book-apis/** folder inside a **.gemini/skills/** directory at the root of the project you have open in Antigravity.
