# Using Custom Deep Learning Models Hosted on Google Colab

In the last two chapters we have written and used Common Lisp client code for the Open AI NLP APIs and the Hugging Face NLP APIs. While this is a relatively easy way to integrate public pretrained deep learning models into your own Common Lisp projects, if you are skilled at writing your own deep learning models you might want to use the material in the earlier chapter **Using Python Deep Learning Models In Common Lisp With a Web Services Interface** to run your models on your own laptop or server and access them through a web service interface.

In this chapter we take a similar approach using the Google Colab service for running Jupyter style notebooks. As an example we will use a [Colab notebook that I wrote that uses both SPARQL queries to DBPedia and a pretrained deep learning question/answering model to answer questions from a user](https://colab.research.google.com/drive/1bM2HWu0sYYsbUpu8X-Hmf4CTuMaSN2wA).

TBD: remove my ngrok token and make it public
