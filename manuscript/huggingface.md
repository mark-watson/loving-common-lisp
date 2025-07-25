# Using the Hugging Face Deep Learning Natural Language Processing APIs

Accessing the HuggingFace NLP APIs is similar to the code we used previously to access the OpenAI and Anthropic APIs.

## History of Hugging Face and How They Differ from OpenAI and Anthropic

[Hugging Face](https://en.wikipedia.org/wiki/Hugging_Face) was founded in 2016 by French entrepreneurs Clement Delangue, Julien Chaumond, and Thomas Wolf. Hugging Face was initially developed as a chatbot application but  open sourcing the model behind the chatbot the company decided to use its experience in developing a platform to run their own models to instead work on a general purpose platform for machine learning. They acquired [Gradio](https://www.gradio.app/guides/quickstart), a software library used to make interactive browser demos of machine learning models. While we won't use Gradio here because it is a Python library and platform, it is worth mentioning that I use Gradio when working on deep learning/LLM projects on Google's Colab.

Comparing Hugging Face with OpenAI and Anthropic, Hugging Face provides an open-source platform for the machine learning community to collaborate on models, datasets, and applications. It supports users in hosting and sharing their own AI models. While OpenAI does allow developers to fine tune models on their platform, I find that Hugging Face to be designed for development and collaboration. Anthropic does not currently support fine tuning their models with your data.

## Common Lisp Library for Hugging Face APIs

The following Common Lisp code is very similar to what we used in the last chapter to call OpenAI's and Anthropic's APIs.

The GitHub repository for this Quicklisp compatible library can be found here:

https://github.com/mark-watson/huggingface


```lisp
(in-package #:huggingface)

;; define the environment variable "HF_API_TOKEN" with the value
;;  of your Hugging Face API key

(defun huggingface-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        json-as-list))))

(defun summarize (some-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl https://api-inference.huggingface.co/models/facebook/bart-large-cnn"
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "HF_API_TOKEN") "\" " 
           " -d '{\"inputs\": \"" some-text "\", \"max_length\": "
           (write-to-string max-tokens) " }'")))
    (cdaar (huggingface-helper curl-command))))

(defun answer-question (question-text context-text)
  (let* ((curl-command
          (concatenate
           'string
           "curl https://api-inference.huggingface.co/models/deepset/roberta-base-squad2"
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "HF_API_TOKEN") "\" " 
           " -d '{\"question\": \"" question-text "\", \"context\": \""
           context-text "\" }'"))
         (answer (huggingface-helper curl-command)))
    (cdar (last answer))))
```

Here are two examples using this code:

```lisp
CL-USER>  (ql:quickload :huggingface)
To load "huggingface":
  Load 1 ASDF system:
    huggingface
; Loading "huggingface"

(:HUGGINGFACE)
CL-USER> (huggingface:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 30)
"Jupiter is the fifth planet from the Sun and the largest in the Solar System. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows. It is on average the third-brightest natural object in the night sky after the Moon and Venus. It has been known to ancient civilizations since before recorded history."

"Jupiter is the fifth planet from the Sun and the largest in the Solar System. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows. It is on average the third-brightest natural object in the night sky after the Moon and Venus. It has been known to ancient civilizations since before recorded history."

CL-USER> (huggingface:answer-question "Where were the 1992 Olympics held?" "The 1992 Summer Games were the first since the end of the Cold War, and the first unaffected by boycotts since the 1972 Summer Games. The 1992 Olympics were in Greece. 1992 was also the first year South Africa was re-invited to the Olympic Games by the International Olympic Committee, after a 32-year ban from participating in international sport.")

"Greece"
CL-USER> 
```

## Hugging Face APIs Wrapup

I believe in supporting Hugging Face because they allow individual developers and smaller organizations to do meaningful custom work LLMs. Hugging Face has emerged as an important public resource in the AI industry by providing an open platform for experimenting with LLMs. This platform has democratized access to AI resources, allowing researchers and developers to collaborate and share LLMs. I like Hugging Face's commitment to open source principles. This is particularly important in an era where there is a growing concern that a few large AI companies might monopolize AI resources. By providing an open platform Hugging Face is acting as an antidote to this potential issue. It ensures that access to cutting-edge AI technology is not restricted to a select few but is available to anyone with the interest and capability to use it. This democratization of AI resources promotes diversity in AI development and helps prevent the concentration of power in a few hands. It encourages a more equitable distribution of AI benefits and mitigates the risks associated with the monopolization of AI technology.
