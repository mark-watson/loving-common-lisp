# Create LLM contexts form huge text datasets for one-shot LLM prompts

Run with more heap space:

```
 sbcl --dynamic-space-size 4096
 ```
 
## This example uses a short Python script tat uses a deep learning transformer model.

You must have the python package manager **uv** install and one time run the python script from the command line to download the required model before trying to run the Common Lisp code:

```bash
$ echo "some text" | uv run generate_embeddings.py
[[-0.012001493945717812, 0.0813082680106163, -0.010090724565088749, 0.011458220891654491, 0.0010405101347714663, 0.06191122904419899, 0.13618092238903046, -0.018939051777124405, 0.12532049417495728, -0.03127983585000038, 0.04177296534180641, 0.056599605828523636,
...
```

This may take a few minutes the first time you run it to download the required model from Hugging Face.
