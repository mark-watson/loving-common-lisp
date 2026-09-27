# Web Scraping

Web scraping, the automated extraction of content from web pages, is a powerful technique for research, data analysis, and building intelligent applications. Before we dive into the code, it is important to discuss responsible and legal web scraping practices. Always read and respect a site's **robots.txt** file and its terms of service before scraping. Limit the rate of your requests so you do not place undue burden on web servers; a delay of a second or two between requests is good practice. Prefer using public APIs when they are available, and only scrape content that is publicly accessible. Be aware that some jurisdictions have laws (such as the Computer Fraud and Abuse Act in the United States or the GDPR in Europe) that restrict automated data collection. When in doubt, contact the site owner for permission. In my own work I routinely email web site owners to explain how I plan to use their data, and this approach has served me well. If you treat web scraping the way you would treat visiting someone's home, politely, and with respect for their resources, you will stay on solid ethical ground.

In this chapter we build three example scripts, found in the **src/webscraping/** directory: **html-headers.lisp**, **page-text.lisp**, and **page-markdown.lisp**. They demonstrate a progression from simple HTML inspection to full content extraction. All three share a helper file, **utils.lisp**, which each example loads first. The examples use the **Drakma** HTTP client to fetch pages and the **Plump** HTML parser to build a DOM tree. The header example also uses **CLSS**, a CSS-selector engine for querying parsed HTML. The cleanup code uses **CL-PPCRE**, a fast regular-expression library. Running any script from the command line loads Quicklisp and its dependencies through `utils.lisp`, so no setup is required.

## Shared Utilities: utils.lisp

{lang="lisp",linenos=on}
~~~~~~~~
;;; Shared helpers for the web-scraping examples in this directory.
;;; Each example loads this file first:
;;;   (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P"")))

(ql:quickload '(:drakma :plump :cl-ppcre) :silent t)

;; DEFPARAMETER rather than DEFCONSTANT so re-loading these examples in one
;; core does not signal a constant-redefinition error.
(defparameter +whitespace+ '(#\Space #\Tab #\Newline #\Return)
  "Characters trimmed from the ends of extracted text.")

(defun trim (string)
  (string-trim +whitespace+ string))

(defun whitespace-only-p (string)
  (every (lambda (char) (member char +whitespace+)) string))

(defun kw (name)
  "Return the keyword symbol for NAME, e.g. \"H1\" => :H1, or NIL."
  (when name (find-symbol (string-upcase name) :keyword)))

(defun text-node-p (node)
  (typep node 'plump:text-node))

(defun element-node-p (node)
  (typep node 'plump:element))

(defun fetch-html (url)
  "Fetch URL, returning the body as a string, or NIL on failure.
Drakma's default User-Agent is blocked by many sites, so we set our own.
When a server omits the charset, Drakma decodes the body with
*DRAKMA-DEFAULT-EXTERNAL-FORMAT* (Latin-1), which garbles UTF-8 text."
  (handler-case
      (drakma:http-request url
                           :user-agent "Mozilla/5.0 (compatible; CL-scraping-example)"
                           :connection-timeout 10
                           :redirect 5)
    (error (c)
      (format *error-output* "~&Could not fetch ~A: ~A~%" url c)
      nil)))

(defun node-text (node)
  "Text of NODE's subtree, skipping comments.
Do not use PLUMP:TEXT on an element for this: its deep walk also collects
comment contents, so hidden text leaks into the output."
  (with-output-to-string (s)
    (labels ((walk (node)
               (cond
                 ((text-node-p node) (write-string (plump:text node) s))
                 ((typep node 'plump:nesting-node)
                  (loop for child across (plump:children node)
                        do (walk child))))))
      (walk node))))

;; NOTE ON REGEX STRINGS: Common Lisp string tokens have no Python-style
;; \n or \t escapes. Inside "...", a backslash merely makes the next
;; character literal, so "\n" is the single character #\n (code 110), not
;; a newline, and "[ \t]" is a class matching space and the letter t.
;; Regex escapes therefore need double backslashes ("\\s+", "[ \\t]+"),
;; and real newline characters must be built from #\Newline (string
;; #\Newline) or ~% (format nil "~A~%") instead of "\n".

(defun normalize-spaces (string)
  "Collapse every run of whitespace, newlines included, into a single space.
Block tags supply the newlines during assembly, so marker tokens (such as
the old __H1__ hack) are not needed to protect them from CLEAN-WHITESPACE."
  (cl-ppcre:regex-replace-all "\\s+" string " "))

(defun raw-text (node)
  "Text of NODE's subtree that collapses spaces/tabs but keeps newlines,
  and skips comments. For <pre>/<code>, where line structure is content."
  (cl-ppcre:regex-replace-all "[ \\t]+" (node-text node) " "))

(defun escape-markdown (string)
  "Escape characters that Markdown would otherwise interpret as syntax."
  (with-output-to-string (s)
    (loop for char across string
          do (when (find char "*_[]`\\") (write-char #\\ s))
             (write-char char s))))

(defun clean-whitespace (text &key strip-indent)
  "Normalize newlines, drop whitespace-only lines, and collapse runs of
blank lines to one. With STRIP-INDENT also remove leading spaces per line
(safe for plain text; Markdown needs them for nested-list indentation).
Text nodes are assumed to be normalized already by NORMALIZE-SPACES, so no
space-collapse pass is needed here."
  ;; Build replacement strings from #\Newline; see the escape warning above.
  (let ((two-newlines (format nil "~A~A" (string #\Newline) (string #\Newline))))
    (when strip-indent
      (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+" text "")))
    (setf text (cl-ppcre:regex-replace-all "(?m)^[ \\t]+$" text ""))
    (setf text (cl-ppcre:regex-replace-all "(?m)[ \\t]+$" text ""))
    ;; Match CRLF and lone CR as single units so Windows-sourced line
    ;; endings collapse the same way Unix ones do.
    (setf text (cl-ppcre:regex-replace-all "(?:\\r\\n|\\r|\\n){3,}" text two-newlines))
    (trim text)))
~~~~~~~~

A few decisions in this file are worth calling out.

`fetch-html` sets a custom User-Agent because Drakma's default is blocked by many sites. It catches errors and returns NIL so one failed page does not stop a larger job. Note that when a server omits the charset from its `Content-Type` header, Drakma decodes the body with Latin-1, which garbles UTF-8 text; see the practice problems at the end of this chapter for one way to recover.

`node-text` walks the subtree by hand instead of calling `plump:text` on an element. The deep walk inside `plump:text` also collects comment contents, so text hidden behind HTML comments leaks into the output.

The comment block above `normalize-spaces` records a trap that catches many people who port regex code to Common Lisp. Lisp string tokens have no `\n` or `\t` escapes, so regex strings need double backslashes (`"\\s+"`) and real newlines must come from `#\Newline` or `~%`.

Earlier versions of these examples wrapped headings in marker tokens like `__H1__` to protect their spacing from the whitespace-collapse pass. That trick is gone. Each text node now goes through `normalize-spaces`, which collapses all whitespace including newlines, and the block tags themselves add the newlines back during assembly. `clean-whitespace` therefore needs no markers.

## Extracting HTML Headers

Our first example, **html-headers.lisp**, is the simplest: fetch a web page and print the text content of every heading tag, `h1` through `h6`. This is useful for quickly surveying the structure of a page, what sections it contains and how the content is organized.

{lang="lisp",linenos=on}
~~~~~~~~
(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))
(ql:quickload :clss :silent t)

(defun fetch-and-print-headers (&optional (url "https://markwatson.com"))
  "Fetch URL and print the text of h1-h6 tags in document order.
One comma-separated selector beats six round trips through the DOM, and
NODE-TEXT keeps HTML comments out of the printed headers."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (let ((nodes (clss:select "h1,h2,h3,h4,h5,h6" (plump:parse html))))
        (if (zerop (length nodes))
            (format t "No h1-h6 headers found.~%")
            (loop for node across nodes
                  for text = (trim (normalize-spaces (node-text node)))
                  unless (string= text "")
                    do (format t "  [~A] ~A~%" (plump:tag-name node) text)))))))

(fetch-and-print-headers)
~~~~~~~~

The flow is straightforward. `fetch-html` downloads the raw HTML as a string, `plump:parse` turns it into a DOM tree, and one `clss:select` call with the comma-separated selector `"h1,h2,h3,h4,h5,h6"` finds every heading in a single pass. CLSS returns nodes in document order, so the output follows the order in which the headings appear on the page. Each heading prints with its tag name in brackets, which preserves the hierarchy in the flat output.

The `(unless (fboundp 'fetch-html) ...)` guard appears in all three examples. It loads `utils.lisp` relative to the file being loaded, so the script works from any working directory, and it skips the load when you have already loaded the utilities in your running image.

You can run this example from the command line:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load html-headers.lisp --eval "(sb-ext:exit)"
~~~~~~~~

Here is a snippet of the output when run against my personal site:

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...
  [h1] Mark Watson
  [h2] Read My Books — Many for Free
  [h2] Connect & Interests
  [h3] Loving Lisp Is Being Rewritten
  [h3] Writings & Blogs
  [h3] Open Source
  [h3] Consulting
  [h4] Additional eBooks on Leanpub
  [h4] Traditional Publications (Springer-Verlag, McGraw-Hill, Morgan Kaufmann)
~~~~~~~~

This tiny script is already quite useful. You could extend it to crawl a list of URLs and build a table of contents for an entire site.

## Extracting Page Content as Plain Text

Our second example, **page-text.lisp**, goes beyond headers and extracts the full readable text of a web page, stripping out scripts, styles, navigation, footers, and other boilerplate. The result is clean plain text suitable for natural language processing, summarization, or feeding into an LLM.

{lang="lisp",linenos=on}
~~~~~~~~
(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))

(defun text-boilerplate-p (tag)
  "Elements dropped for plain-text extraction. The Markdown example keeps
nav/header/footer because their link lists carry meaning; here we want only
the article body. Named differently from MD-BOILERPLATE-P so both examples
can be loaded in one core without clobbering each other."
  (member tag '("script" "style" "head" "nav" "header" "footer" "iframe" "noscript")
          :test #'string-equal))

(defun get-element-spacing (tag text)
  "Wrap TEXT in newlines appropriate for TAG. CLEAN-WHITESPACE collapses
over-long runs afterward, so each block only needs enough newlines to
separate it from its neighbors."
  (case (kw tag)
    ((:h1 :h2 :h3 :h4 :h5 :h6 :p :blockquote :ul :ol :pre)
     (format nil "~%~A~%~%" text))
    ((:li :tr)
     (format nil "~A~%" text))
    (:br
     (format nil "~%"))
    ((:div :article :section :aside :main)
     (if (and (plusp (length text))
              (char= #\Newline (char text (1- (length text)))))
         text
         (format nil "~A~%" text)))
    (t text)))

(defun children-text (node)
  (with-output-to-string (s)
    (loop for child across (plump:children node)
          do (write-string (get-clean-text child) s))))

(defun get-clean-text (node)
  "Recursively collect text from NODE, skipping boilerplate tags and
inserting line breaks for block tags. <pre>/<code> keep their internal
newlines; all other text is whitespace-normalized at the node level."
  (cond
    ((text-node-p node)
     (normalize-spaces (plump:text node)))
    ((element-node-p node)
     (let ((tag (plump:tag-name node)))
       (if (text-boilerplate-p tag)
           ""
           (let ((text (if (member (kw tag) '(:pre :code))
                           (raw-text node)
                           (children-text node))))
             (get-element-spacing tag text)))))
    ((typep node 'plump:nesting-node)
     (children-text node))
    (t "")))

(defun fetch-and-print-text (&optional (url "https://markwatson.com"))
  "Fetch URL and print cleaned-up text content."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (format t "~A~%"
              (clean-whitespace (get-clean-text (plump:parse html))
                                :strip-indent t)))))

(fetch-and-print-text)
~~~~~~~~

The `text-boilerplate-p` function filters out tags that contain non-content material: `script`, `style`, `head`, `nav`, `header`, `footer`, `iframe`, and `noscript`. `get-element-spacing` maps each HTML element to the appropriate whitespace treatment: headings, paragraphs, and lists get blank lines around them, list items and table rows get one newline after them, and container elements like `div` get a newline only when their text does not already end with one.

`get-clean-text` walks the tree recursively. It calls `normalize-spaces` on each text node, which collapses every run of whitespace, including source-formatting newlines inside a paragraph, into a single space. Block tags then supply the real newlines. The `kw` helper converts a tag name to a keyword so the dispatch code can use `case` instead of long chains of `string-equal` tests. The one exception to the collapse-everything rule: `<pre>` and `<code>` content goes through `raw-text`, which keeps newlines because line structure is part of the content of those elements.

The plain-text version wants only the article body, so it drops `nav`, `header`, and `footer`. The Markdown example in the next section keeps them. Because both examples can be loaded into the same Lisp image, the two boilerplate predicates carry different names, `text-boilerplate-p` and `md-boilerplate-p`, so neither clobbers the other.

The final call passes `:strip-indent t` to `clean-whitespace`, which removes leading spaces from each line. Those spaces carry no meaning in plain text.

You can run this example from the command line:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load page-text.lisp --eval "(sb-ext:exit)"
~~~~~~~~

Sample output (truncated):

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...
Mark Watson

AI Generalist

Self-funded open source Artificial Intelligence researcher and author. Engineering AI systems, knowledge graphs, and distributed architectures for organizations like Google, Mind AI, Capital One, Disney, SAIC, Baby List, Olive and CompassLabs.

40+ Years in AI
20+ Books Published
55 US Patents

Explore My Books Connect with Me

Work Experience
Mostly AI projects since 1985. Large scale distributed systems for DARPA and PacBell. Knowledge Graphs at Google. Master Software Engineer and managed an AI team at Capital One. 20 years in the defense industry at SAIC/Leidos.

Books
Read My Books — Many for Free

All of my recent books are available on Leanpub. Most can be read online at no cost.
~~~~~~~~

## Converting a Web Page to Markdown

Our third and most complete example, **page-markdown.lisp**, converts a full web page into well-formed Markdown. This is useful for feeding web content into large language models, which process Markdown far more effectively than raw HTML.

{lang="lisp",linenos=on}
~~~~~~~~
(unless (fboundp 'fetch-html)
  (load (merge-pathnames #P"utils.lisp" (or *load-pathname* #P""))))

(defun md-boilerplate-p (tag)
  "Elements removed before Markdown conversion. Unlike page-text.lisp,
nav/header/footer are kept: their link lists are often the point of the
conversion. Named apart from TEXT-BOILERPLATE-P so both examples can be
loaded in one core without clobbering each other."
  (member (kw tag) '(:script :style :head :noscript :iframe) :test #'eq))

(defun md-heading (level inner)
  (format nil "~%~A ~A~%~%"
          (make-string level :initial-element #\#) (trim inner)))

(defun whitespace-child-p (child)
  "Between <li> tags, text nodes are source indentation, not content."
  (and (text-node-p child) (whitespace-only-p (plump:text child))))

(defun html-to-markdown (node &optional (depth 0) (ordered nil) (index 1))
  "Recursively convert the plump HTML node NODE into Markdown.
DEPTH counts enclosing lists (nested items indent two spaces per level),
ORDERED is true inside an <ol>, and INDEX is an <li>'s position within its
list. Text nodes are Markdown-escaped and whitespace-normalized up front;
block tags contribute the newlines, so CLEAN-WHITESPACE needs no marker
tokens to protect spacing."
  (cond
    ((text-node-p node)
     (normalize-spaces (escape-markdown (plump:text node))))
    ((element-node-p node)
     (let ((tag (kw (plump:tag-name node))))
       (cond
         ((md-boilerplate-p tag) "")
         ((member tag '(:ul :ol) :test #'eq)
          (with-output-to-string (s)
            (let ((item 0))
              (loop for child across (plump:children node)
                    unless (whitespace-child-p child)
                      do (incf item)
                         (write-string (html-to-markdown child (1+ depth)
                                                         (eq tag :ol) item)
                                       s)))))
         (t
          (let ((inner (if (member tag '(:pre :code) :test #'eq)
                           (raw-text node)
                           (with-output-to-string (s)
                             (loop for child across (plump:children node)
                                   do (write-string (html-to-markdown child depth
                                                                      ordered index)
                                                    s))))))
            (case tag
              (:h1 (md-heading 1 inner))
              (:h2 (md-heading 2 inner))
              (:h3 (md-heading 3 inner))
              (:h4 (md-heading 4 inner))
              (:h5 (md-heading 5 inner))
              (:h6 (md-heading 6 inner))
              ((:p :blockquote)
               (format nil "~%~A~%~%" (trim inner)))
              (:br
               (format nil "~%~A" inner))
              ((:strong :b)
               (format nil "**~A**" inner))
              ((:em :i)
               (format nil "*~A*" inner))
              (:code
               (format nil "`~A`" (trim inner)))
              (:pre
               (format nil "~%~%```~%~A~%```~%~%" (trim inner)))
              (:a
               (let ((href (plump:attribute node "href")))
                 (if (and href (plusp (length (trim inner))))
                     (format nil "[~A](~A)" (trim inner) href)
                     inner)))
              (:img
               (let ((src (plump:attribute node "src"))
                     (alt (escape-markdown (or (plump:attribute node "alt") ""))))
                 (if src
                     (format nil "![~A](~A)" alt src)
                     "")))
              (:li
               (let ((indent (make-string (* 2 (max 0 (1- depth)))
                                          :initial-element #\Space))
                     (marker (if ordered (format nil "~D. " index) "* ")))
                 (format nil "~A~A~A~%" indent marker (trim inner))))
              ((:div :article :section :aside :main :tr :table :td :th)
               (format nil "~%~A~%~%" inner))
              (t inner)))))))
    ((typep node 'plump:nesting-node)
     (with-output-to-string (s)
       (loop for child across (plump:children node)
             do (write-string (html-to-markdown child depth) s))))
    (t "")))

(defun fetch-and-print-markdown (&optional (url "https://markwatson.com"))
  "Fetch URL and print the content converted to Markdown."
  (format t "Fetching ~A...~%" url)
  (let ((html (fetch-html url)))
    (when html
      (format t "~A~%"
              (clean-whitespace (html-to-markdown (plump:parse html)))))))

(fetch-and-print-markdown)
~~~~~~~~

`html-to-markdown` walks the DOM tree recursively. For each element node it first converts all children to Markdown (the `inner` string), then wraps the result in the appropriate Markdown syntax based on the tag: `#` prefixes for headings, `**...**` for bold, `*...*` for italic, `[text](url)` for links, `![alt](src)` for images, `* ` for list items, and triple backticks for code blocks. `md-heading` builds the heading prefix with `make-string`, so one helper serves all six heading levels.

Three details separate this converter from a naive tag-to-syntax mapping.

First, `html-to-markdown` carries three extra arguments: `depth` counts enclosing lists so nested items indent two spaces per level, `ordered` records whether the current list is an `<ol>`, and `index` is an item's position in its list, which produces the `1.`, `2.`, `3.` markers of ordered lists. The `:ul`/`:ol` branch increments a counter for each non-whitespace child and passes that count down as the item's index. `whitespace-child-p` skips the text nodes that sit between `<li>` tags in the source HTML; they are indentation, not content, and counting them would offset every list number.

Second, every text node passes through `escape-markdown`, which backslash-escapes characters like `*`, `_`, and brackets. Without this step, a page that sells "20% off *limited time*" would turn part of its own text into Markdown emphasis.

Third, this example keeps `nav`, `header`, and `footer` elements that the plain-text version drops, because on many sites the link lists in those regions are the point of the conversion. It also differs in one cleanup setting: `fetch-and-print-markdown` calls `clean-whitespace` without `:strip-indent`, because leading spaces are how Markdown represents nested list indentation.

You can run this example from the command line:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load page-markdown.lisp --eval "(sb-ext:exit)"
~~~~~~~~

Sample output (truncated):

{linenos=off}
~~~~~~~~
Fetching https://markwatson.com...
[Mark Watson](#)    * [Books](#books)
* [Connect](#connect)
* [Flagstaff Arizona](/flagstaff.html)
* [Blogspot](https://mark-watson.blogspot.com/)
* [Substack](https://marklwatson.substack.com)
* [GitHub](https://github.com/mark-watson)
* [Leanpub](https://leanpub.com/u/markwatson)

 ![Mark Watson portrait](/pictures/Mark_small_for_twitter.jpg)
# Mark Watson

****AI Generalist****

Self-funded open source Artificial Intelligence researcher and author. Engineering AI systems, knowledge graphs, and distributed architectures for organizations like **Google**, **Mind AI**, **Capital One**, **Disney**, **SAIC**, **Baby List**, **Olive** and **CompassLabs**.
~~~~~~~~

## Wrap Up

The scripts in this chapter form a practical toolkit for extracting information from the web using Common Lisp. The header extractor gives you a quick structural overview, the plain-text extractor yields clean readable content, and the Markdown converter produces richly formatted output ideal for downstream processing. The shared `utils.lisp` file shows how to factor out the concerns that every scraper faces: fetching with a proper User-Agent, skipping HTML comments, and normalizing whitespace.

Here are some project ideas that build on this web scraping code:

- **Build a personal knowledge base.** Scrape articles and blog posts you read frequently, convert them to Markdown, and store them in a local file system or database for full-text search. This is especially powerful when combined with the embedding and vector search techniques covered in later chapters.
- **Create a site-structure analyzer.** Extend the header extraction script to crawl an entire site (following internal links) and build a hierarchical table of contents. This is invaluable for auditing large documentation sites or wikis.
- **Feed web content to an LLM.** Use the Markdown converter to scrape a page and pass the cleaned output directly to an LLM API (such as the OpenAI, Ollama, or Gemini interfaces covered elsewhere in this book) for summarization, question answering, or translation.
- **Monitor pages for changes.** Run the plain-text extractor on a schedule and diff successive snapshots to detect when a page's content changes, useful for tracking product prices, news updates, or government filings.
- **Extract structured data.** Adapt the CSS-selector technique from the header example to pull specific data fields (prices, dates, names) from pages with consistent HTML structure, and export the results as CSV or JSON.
- **Combine with the Lightpanda browser client.** For pages that require JavaScript rendering, use the Lightpanda interface from the previous chapter to fetch the fully rendered HTML, then pass that HTML through the text or Markdown extraction functions developed here.

## Optional Practice Problems

1. **Extraction of Hyperlinks into an Association List**:
   In [page-text.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/page-text.lisp), hyperlink tags `<a>` are processed by simply extracting their inner text content. Write a function `extract-all-links` that parses the DOM tree using CLSS and extracts all `href` attributes, converting them into an association list of `(anchor-text . url)`. Filter out empty anchors or relative links, resolving relative paths against the base URL.

2. **Robots.txt Parser and Rate Limiting Compliance**:
   To comply with the responsible scraping guidelines mentioned in the introduction of [web-scraping.md](file:///Users/markw/GITHUB/loving-common-lisp/manuscript/web-scraping.md), write a utility function `allowed-by-robots-p` in `utils.lisp` or as a helper. Fetch and parse the `/robots.txt` file for a given URL, check if the current user agent is allowed to access the target path, and read any `Crawl-delay` directive to sleep dynamically before making requests via Drakma.

3. **Markdown Table Converter**:
   The `html-to-markdown` function in [page-markdown.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/page-markdown.lisp) renders table cells as separated blocks, but does not produce GitHub Flavored Markdown (GFM) tables. Extend the `:table` and `:tr` branches to collect rows, detect header cells (`<th>`), and emit a correctly formatted GFM table with a header separator row.

4. **User-Agent Rotation and Request Header Customization**:
   `fetch-html` in [utils.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/utils.lisp) sends a single fixed User-Agent. Change it to randomly select a User-Agent string from a pre-defined list of modern browser headers (Chrome, Firefox, Safari) and to add custom request headers like `Accept-Language` or `Referer` to the Drakma request.

5. **Recursive Site Crawler with Level Limits (DFS/BFS)**:
   Build a recursive site crawler in [page-text.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/page-text.lisp) that starts from a seed URL, extracts all internal links (using your link extraction function), and recursively scrapes pages up to a maximum depth limit (e.g. depth 2). Store the scraped pages as individual Markdown files, keeping track of visited URLs in a hash table to avoid infinite recursion.

6. **Automatic Encoding Detection and Recovery**:
   `fetch-html` in [utils.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/utils.lisp) notes that Drakma falls back to Latin-1 when a server omits the charset, which garbles UTF-8 text. Write a wrapper function `fetch-html-with-encoding-recovery` that requests the body as bytes (`:want-stream nil` combined with a Latin-1 external format, or `drakma:http-request` with `:external-format` overridden), searches the raw bytes for `<meta charset="...">` tags, and re-decodes the byte array using `flexi-streams:octets-to-string` with the detected encoding.
