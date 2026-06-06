# Web Scraping

Web scraping—the automated extraction of content from web pages—is a powerful technique for research, data analysis, and building intelligent applications. Before we dive into the code, it is important to discuss responsible and legal web scraping practices. Always read and respect a site's **robots.txt** file and its terms of service before scraping. Limit the rate of your requests so you do not place undue burden on web servers; a delay of a second or two between requests is good practice. Prefer using public APIs when they are available, and only scrape content that is publicly accessible. Be aware that some jurisdictions have laws (such as the Computer Fraud and Abuse Act in the United States or the GDPR in Europe) that restrict automated data collection. When in doubt, contact the site owner for permission. In my own work I routinely email web site owners to explain how I plan to use their data, and this approach has served me well. If you treat web scraping the way you would treat visiting someone's home—politely, and with respect for their resources—you will stay on solid ethical ground.

In this chapter we develop three self-contained scripts, found in the **src/webscraping/** directory: **html-headers.lisp**, **page-text.lisp**, and **page-markdown.lisp**. Together they demonstrate a progression from simple HTML inspection to full content extraction. All three examples use the **Drakma** HTTP client to fetch pages and the **Plump** HTML parser to build a DOM tree. The third library we use is **CLSS**, a CSS-selector engine for querying parsed HTML. For text cleanup the examples use **CL-PPCRE**, a fast regular-expression library. Each script loads its dependencies via Quicklisp at the top of the file so you can run them directly from the command line.

## Extracting HTML Headers

Our first example, **html-headers.lisp**, is the simplest: fetch a web page and print the text content of every heading tag, `h1` through `h6`. This is useful for quickly surveying the structure of a page—what sections does it contain, and how is the content organized?

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload '(:drakma :plump :clss))

(defun fetch-and-print-headers (url)
  "Fetches the URL and prints text from H1 to H6 tags using drakma, plump, and clss."
  (format t "Fetching ~A...~%" url)
  (let* ((html-content (drakma:http-request url))
         (parsed-html (plump:parse html-content)))
    (dolist (tag '("h1" "h2" "h3" "h4" "h5" "h6"))
      (format t "~A sections:~%" tag)
      (let ((nodes (clss:select tag parsed-html)))
        (loop for node across nodes do
          (let ((text (plump:text node)))
            (when text
              (format t "  - ~A~%"
                (string-trim '(#\Space #\Tab #\Newline #\Return) text)))))))))

(fetch-and-print-headers "https://markwatson.com")
~~~~~~~~

The flow is straightforward. Line 6 calls `drakma:http-request` to download the raw HTML as a string. Line 7 parses it into a Plump DOM tree. We then loop over each heading level (line 8), use `clss:select` with a CSS selector string to find matching nodes (line 10), and extract the text content with `plump:text` (line 12). The `string-trim` call on line 15 strips leading and trailing whitespace from each heading's text.

You can run this example from the command line:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load html-headers.lisp --eval "(sb-ext:exit)"
~~~~~~~~

Here is a snippet of the output when run against my personal site:

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...
h1 sections:
  - Mark Watson: AI Practitioner and Author
h2 sections:
  - Current Projects
  - Free Books
  - Commercial Books
h3 sections:
h4 sections:
h5 sections:
h6 sections:
~~~~~~~~

This tiny script is already quite useful. You could extend it to crawl a list of URLs and build a table of contents for an entire site.


## Extracting Page Content as Plain Text

Our second example, **page-text.lisp**, goes beyond headers and extracts the full readable text of a web page, stripping out scripts, styles, navigation, footers, and other boilerplate. The result is clean plain text suitable for natural language processing, summarization, or feeding into an LLM.

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload '(:drakma :plump :cl-ppcre))

(defun text-node-p (node)
  (typep node 'plump:text-node))

(defun element-node-p (node)
  (typep node 'plump:element))

(defun ignore-tag-p (tag)
  (member tag '("script" "style" "head" "nav" "header"
                "footer" "iframe" "noscript")
          :test #'string-equal))

(defun get-element-spacing (tag text)
  "Returns the text wrapped in appropriate layout formatting
   or markers based on the tag."
  (cond
    ((string-equal tag "h1")
      (format nil "~%__H1__~%~A~%__H1__~%" text))
    ((string-equal tag "h2")
      (format nil "~%__H2__~%~A~%__H2__~%" text))
    ((member tag '("h3" "h4" "h5" "h6") :test #'string-equal)
      (format nil "~%~A~%~%" text))
    ((member tag '("p" "blockquote" "pre" "ul" "ol")
             :test #'string-equal)
      (format nil "~%~A~%~%" text))
    ((string-equal tag "br")
      (format nil "~A~%" text))
    ((member tag '("li" "tr") :test #'string-equal)
      (format nil "~A~%" text))
    ((member tag '("div" "article" "section" "aside" "main")
             :test #'string-equal)
      (if (and (> (length text) 0)
               (not (char= (char text (1- (length text)))
                            #\Newline)))
          (format nil "~A~%" text)
          text))
    (t text)))

(defun get-clean-text (node)
  "Recursively collects text from NODE, skipping non-content
   tags and inserting linebreaks/markers for tags."
  (cond
    ((text-node-p node)
      (plump:text node))
    ((element-node-p node)
      (let ((tag (plump:tag-name node)))
        (if (ignore-tag-p tag)
            ""
            (let ((text (with-output-to-string (s)
                          (loop for child across (plump:children node)
                                do (write-string
                                     (get-clean-text child) s)))))
              (get-element-spacing tag text)))))
    ((typep node 'plump:nesting-node)
      (with-output-to-string (s)
        (loop for child across (plump:children node)
              do (write-string (get-clean-text child) s))))
    (t "")))
~~~~~~~~

The key design decision here is the `ignore-tag-p` function (line 9) which filters out tags that contain non-content material: `script`, `style`, `head`, `nav`, `header`, `footer`, `iframe`, and `noscript`. The `get-element-spacing` function (line 14) maps each HTML element to the appropriate whitespace treatment—headings get extra blank lines, paragraphs get double newlines, list items get single newlines, and so on.

The marker strings `__H1__` and `__H2__` are a clever technique: they act as placeholders that the whitespace cleanup pass (below) can later replace with the correct number of blank lines, without being collapsed during the intermediate cleanup steps.

{lang="lisp",linenos=on}
~~~~~~~~
(defun clean-whitespace (text)
  "Cleans up excessive spaces and newlines in the text,
   preserving spacing for H1/H2."
  (let* ((n (format nil "~%"))
         ;; 1. Clean up lines that contain only whitespace
         (text (cl-ppcre:regex-replace-all
                 "(?m)^[ \\t]+$" text ""))
         ;; 2. Collapse double spaces
         (text (cl-ppcre:regex-replace-all
                 "[ \\t]+" text " "))
         ;; 3. Collapse 3+ newlines to at most 2
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A{3,}" n)
                 text (format nil "~A~A" n n)))
         ;; 4. Replace __H1__ markers with 4 newlines
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A*__H1__~A*" n n)
                 text (format nil "~A~A~A~A" n n n n)))
         ;; 5. Replace __H2__ markers with 3 newlines
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A*__H2__~A*" n n)
                 text (format nil "~A~A~A" n n n)))
         ;; 6. Trim leading/trailing whitespace
         (text (string-trim
                 '(#\Space #\Tab #\Newline #\Return) text)))
    text))

(defun fetch-and-print-text (url)
  "Fetches the URL and prints cleaned up text content."
  (format t "Fetching ~A...~%" url)
  (let* ((html-content (drakma:http-request url))
         (parsed-html (plump:parse html-content))
         (raw-text (get-clean-text parsed-html))
         (cleaned-text (clean-whitespace raw-text)))
    (format t "~A~%" cleaned-text)))

(fetch-and-print-text "https://markwatson.com")
~~~~~~~~

The `clean-whitespace` function applies five regular-expression passes using CL-PPCRE: strip whitespace-only lines, collapse runs of spaces, collapse excessive newlines, expand the `__H1__` and `__H2__` markers into proper visual spacing, and finally trim the whole result.

Sample output (truncated):

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...



Mark Watson: AI Practitioner and Author



Current Projects

I am currently working on the 6th edition of my book
"Loving Common Lisp, or the Savvy Programmer's Secret Weapon."
I also develop AI-powered applications using Common Lisp,
Haskell, and Python.



Free Books

Loving Common Lisp, or the Savvy Programmer's Secret Weapon
A Lisp Programmer Living in Python-Land: The Hy Programming Language
Practical Artificial Intelligence Programming With Clojure
~~~~~~~~


## Converting a Web Page to Markdown

Our third and most sophisticated example, **page-markdown.lisp**, converts a full web page into well-formed Markdown. This is especially useful for feeding web content into large language models, which process Markdown far more effectively than raw HTML.

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload '(:drakma :plump :cl-ppcre))

(defun text-node-p (node)
  (typep node 'plump:text-node))

(defun element-node-p (node)
  (typep node 'plump:element))

(defun ignore-tag-p (tag)
  (member tag '("script" "style" "head" "noscript" "iframe")
          :test #'string-equal))

(defun block-element-p (tag)
  (member tag '("p" "div" "li" "br" "h1" "h2" "h3" "h4"
                "h5" "h6" "tr" "article" "section" "aside")
          :test #'string-equal))

(defun html-to-markdown (node)
  "Recursively converts a plump HTML node into Markdown."
  (cond
    ((text-node-p node)
      (plump:text node))
    ((element-node-p node)
      (let* ((tag (plump:tag-name node))
             (inner-md
               (with-output-to-string (s)
                 (loop for child across (plump:children node)
                       do (write-string
                            (html-to-markdown child) s)))))
        (cond
          ((ignore-tag-p tag) "")
          ((string-equal tag "h1")
            (format nil "~%__H1__~%# ~A~%__H1__~%"
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           inner-md)))
          ((string-equal tag "h2")
            (format nil "~%__H2__~%## ~A~%__H2__~%"
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           inner-md)))
          ((string-equal tag "h3")
            (format nil "~%### ~A~%~%"
              (string-trim '(#\Space #\Tab) inner-md)))
          ((string-equal tag "h4")
            (format nil "~%#### ~A~%~%"
              (string-trim '(#\Space #\Tab) inner-md)))
          ((string-equal tag "h5")
            (format nil "~%##### ~A~%~%"
              (string-trim '(#\Space #\Tab) inner-md)))
          ((string-equal tag "h6")
            (format nil "~%###### ~A~%~%"
              (string-trim '(#\Space #\Tab) inner-md)))
          ((string-equal tag "p")
            (format nil "~%~A~%~%"
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           inner-md)))
          ((string-equal tag "br") (format nil "~%"))
          ((string-equal tag "strong")
            (format nil "**~A**" inner-md))
          ((string-equal tag "b")
            (format nil "**~A**" inner-md))
          ((string-equal tag "em")
            (format nil "*~A*" inner-md))
          ((string-equal tag "i")
            (format nil "*~A*" inner-md))
          ((string-equal tag "code")
            (format nil "`~A`" inner-md))
          ((string-equal tag "pre")
            (format nil "~%```~%~A~%```~%" inner-md))
          ((string-equal tag "a")
            (let ((href (plump:attribute node "href")))
              (if (and href
                       (> (length (string-trim
                                    '(#\Space #\Tab)
                                    inner-md)) 0))
                  (format nil "[~A](~A)"
                    (string-trim
                      '(#\Space #\Tab #\Newline #\Return)
                      inner-md)
                    href)
                  inner-md)))
          ((string-equal tag "img")
            (let ((src (plump:attribute node "src"))
                  (alt (or (plump:attribute node "alt")
                           "image")))
              (if src
                  (format nil "![~A](~A)" alt src)
                  "")))
          ((string-equal tag "li")
            (format nil "* ~A~%"
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           inner-md)))
          ((block-element-p tag)
            (format nil "~%~A~%" inner-md))
          (t inner-md))))
    ((typep node 'plump:nesting-node)
      (with-output-to-string (s)
        (loop for child across (plump:children node)
              do (write-string (html-to-markdown child) s))))
    (t "")))
~~~~~~~~

The `html-to-markdown` function is the heart of this example. It walks the DOM tree recursively. For each element node it first converts all children to Markdown (the `inner-md` string), then wraps the result in the appropriate Markdown syntax based on the tag: `#` prefixes for headings, `**...**` for bold, `*...*` for italic, `[text](url)` for links, `![alt](src)` for images, `* ` for list items, and triple backticks for code blocks.

The whitespace cleanup function is nearly identical to the plain-text version:

{lang="lisp",linenos=on}
~~~~~~~~
(defun clean-markdown-whitespace (text)
  "Cleans up excessive spaces and newlines in the Markdown,
   preserving spacing for H1/H2."
  (let* ((n (format nil "~%"))
         (text (cl-ppcre:regex-replace-all
                 "(?m)^[ \\t]+$" text ""))
         (text (cl-ppcre:regex-replace-all
                 "[ \\t]+" text " "))
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A{3,}" n)
                 text (format nil "~A~A" n n)))
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A*__H1__~A*" n n)
                 text (format nil "~A~A~A~A" n n n n)))
         (text (cl-ppcre:regex-replace-all
                 (format nil "~A*__H2__~A*" n n)
                 text (format nil "~A~A~A" n n n)))
         (text (string-trim
                 '(#\Space #\Tab #\Newline #\Return) text)))
    text))

(defun fetch-and-print-markdown (url)
  "Fetches the URL and prints content converted to Markdown."
  (format t "Fetching ~A...~%" url)
  (let* ((html-content (drakma:http-request url))
         (parsed-html (plump:parse html-content))
         (raw-markdown (html-to-markdown parsed-html))
         (cleaned-markdown
           (clean-markdown-whitespace raw-markdown)))
    (format t "~A~%" cleaned-markdown)))

(fetch-and-print-markdown "https://markwatson.com")
~~~~~~~~

Sample output (truncated):

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...



# Mark Watson: AI Practitioner and Author



## Current Projects

I am currently working on the 6th edition of my book
**Loving Common Lisp, or the Savvy Programmer's Secret Weapon.**
I also develop AI-powered applications using Common Lisp,
Haskell, and Python.



## Free Books

* [Loving Common Lisp](https://leanpub.com/lovinglisp)
* [A Lisp Programmer Living in Python-Land](https://leanpub.com/hy-lisp-python)
~~~~~~~~


## Wrap Up

The three scripts in this chapter form a practical toolkit for extracting information from the web using Common Lisp. The header extractor gives you a quick structural overview, the plain-text extractor yields clean readable content, and the Markdown converter produces richly formatted output ideal for downstream processing.

Here are some project ideas that build on this web scraping code:

- **Build a personal knowledge base.** Scrape articles and blog posts you read frequently, convert them to Markdown, and store them in a local file system or database for full-text search. This is especially powerful when combined with the embedding and vector search techniques covered in later chapters.
- **Create a site-structure analyzer.** Extend the header extraction script to crawl an entire site (following internal links) and build a hierarchical table of contents. This is invaluable for auditing large documentation sites or wikis.
- **Feed web content to an LLM.** Use the Markdown converter to scrape a page and pass the cleaned output directly to an LLM API (such as the OpenAI, Ollama, or Gemini interfaces covered elsewhere in this book) for summarization, question answering, or translation.
- **Monitor pages for changes.** Run the plain-text extractor on a schedule and diff successive snapshots to detect when a page's content changes—useful for tracking product prices, news updates, or government filings.
- **Extract structured data.** Adapt the CSS-selector technique from the header example to pull specific data fields (prices, dates, names) from pages with consistent HTML structure, and export the results as CSV or JSON.
- **Combine with the Lightpanda browser client.** For pages that require JavaScript rendering, use the Lightpanda interface from the previous chapter to fetch the fully rendered HTML, then pass that HTML through the text or Markdown extraction functions developed here.
