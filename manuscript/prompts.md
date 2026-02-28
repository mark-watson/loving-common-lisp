# Prompt Engineering for Large Language Models

We have been using prompt engineering in an informal way in the last few chapters. In this chapter we look at prompt engineering in more detail. My hope is that, dear reader, you find this chapter to be both a short reference for Large Language Models (LLMS) and a useful source of prompt examples at the end of the chapter.

## Two Types of LLMS

In order to avoid "model hallucinations" (i.e., models making up answers with no basis in fact) when using Large Language Models (LLMS) it is important to explain in detail instructions and add context text to help the model better answer questions of solve problems.

Base LLMs and Instruction Tuned LLMs are both types of advanced AI language models. While they share some similarities in their underlying architecture and training processes, they have distinct differences in terms of their fine-tuning and usage. Here, we will discuss the similarities and differences in detail.

The similarities are:

- Architecture: Both models are based on the Transformer architecture, which has been shown to be highly effective for various natural language processing tasks. This architecture relies on self-attention mechanisms to process input data and generate outputs.
- Pretraining: Both models undergo a pretraining phase where they learn from a large corpus of text, such as websites, books, and articles. This helps the models acquire a general understanding of language, grammar, facts, and some reasoning abilities.
- Transfer learning: Both models can use transfer learning, which involves fine-tuning the pretrained models on specific tasks or datasets. This process adapts the model's general knowledge to the target application, improving its performance on the desired task.

The differences are:

- Fine-tuning objectives: The key difference between the two models lies in their fine-tuning process. Base Large Language Models are fine-tuned to generate human-like text with minimal explicit guidance, whereas Instruction Tuned Large Language Models are specifically optimized to follow instructions provided in the input prompt.
- Task performance: Due to their different fine-tuning objectives, Instruction Tuned Large Language Models tend to perform better on tasks that require following explicit instructions, while Base Large Language Models may produce more diverse and creative responses that may not always adhere to the given instructions.
- Usage: Base Large Language Models can be used for a wide variety of tasks, including text generation, summarization, translation, and question-answering. Instruction Tuned Large Language Models, on the other hand, are designed to be more controllable and responsive to input instructions, making them more suitable for applications that require precise output based on specific instructions.
- Prompt engineering: To get the desired output from a Base Large Language Model, users often need to carefully craft prompts, which can be a trial-and-error process. With Instruction Tuned Large Language Models, users can provide more explicit instructions to achieve the desired output more consistently.

In summary, both Base and Instruction Tuned Large Language Models share the same foundation in terms of architecture and pretraining but differ in their fine-tuning objectives and application strengths. While Base Large Language Models generate more diverse and creative text, Instruction Tuned Large Language Models excel at tasks that require following specific instructions and are more controllable.

## Prompt Examples

We have used both OpenAI and Hugging Face Models so far in this book. When you practice prompt engineering and learn new techniques, your knowledge of prompt engineering can be applied to the OpenAI and Hugging Face APIs and models.

As Andrew Ng and Isa Fulford point out in their one hour class [ChatGPT Prompt Engineering for Developers](https://learn.deeplearning.ai/chatgpt-prompt-eng/lesson), you can craft prompts as if you were instructing an intelligent person who is not necessarily familiar with the current problem you are trying to solve with prompt engineering. There is a bit of a tradeoff here: you pay per input and output tokens with the OpenAI APIs so you may be tempted to compress your context text, instructions, and questions when creating prompts. However, in general to more context and information you can provide increases the quality of the resulting output.

The following sections show examples of prompt engineering that I use in my own work. In order to learn more techniques I recommend also performing web search for prompt engineering with a description of the problem you are working on. As an example, I asked Microsoft Bing+ChatGPT in a chat to help me with **I need help with prompt engineering using the OpenAI APIs. My problem is: I have a text file that contains customer names and phone numbers. I need a prompt to list names and phone numbers.** and the advice was a useful starting point. In a similar way I received useful advice when chatting with Google Bard.

### Find Entities and Relationships

Prompt:

```
The task is to extract as many relevant entities and the relationships between these entities.\n
When possible, return the type of an entity using the Wikipedia class system.\n
Extract all relevant relationships between identified entities.\n
The relationships should follow the Wikipedia schema type.
The output of a relationship should be in a form of a triple Head, Relationship, Tail, for example
Bill Gates, BORN_AT, Seatle.\n
As an example "Bill Gates worked at Microsoft" should have an output with the following format:\n
entity
Bill Gates, person
Microsoft, organization

relationships
Bill Gates, WORKET AT, Microsoft\n
Task: process this text:
Steve Jobes founded Apple Computer and Bill Gates started Microsoft. Seattle is in the state of Washington.
Steve lived in Palo Alto. Palo Alto is located in California.
```

ChatGPT 4 returns:

```
entities
Steve Jobs, person
Apple Computer, organization
Bill Gates, person
Microsoft, organization
Seattle, city
Washington, administrative territorial entity

relationships
Steve Jobs, FOUNDED, Apple Computer
Bill Gates, FOUNDED, Microsoft
Seattle, IS_IN, Washington
```

### Disambiguate Relationship Names

Prompt:

```
Given a list of relationships like:

brother
located in
sister
sibling
found at
father
location
mother
relative

Then return a list with an index for each unique relationship, like:

brother, 1
located in, 2
sister, 1
sibling, 1
found at, 2
father, 1
location, 2
mother, 1
relative, 1

Task: process the following text:

Sam has a brother Mike. Suzy has a sister Sally. Suszy ahs a mother Jill, and a father Bill.
Mike is in San Diego. Sam is located is Oakland. Suzy's family all live in Los Angeles.
```

ChatGPT 4 results are:

```
Based on the provided list of relationships, here is the processed text with an index for each unique relationship:

Sam has a brother Mike. (brother, 1)
Suzy has a sister Sally. (sister, 1)
Suzy has a mother Jill, and a father Bill. (mother, 1; father, 1)
Mike is in San Diego. (located in, 2)
Sam is located in Oakland. (located in, 2)
Suzy's family all live in Los Angeles. (location, 2)

Here, relationships like "brother," "sister," "mother," and "father" are assigned an index of 1, while relationships like "located in" and "location" are assigned an index of 2.
```

### Disambiguate Relationship Names and Rewrite Text

Prompt:

```
We want to disambiguate relationship names and then rewrite text using canonical relationship names.

Given a list of relationships like:

brother
located in
sister
sibling
found at
father
location
mother
relative

Then generate a list with an index for each unique relationship, like:

brother, 1
located in, 2
sister, 1
sibling, 1
found at, 2
father, 1
location, 2
mother, 1
relative, 1

And then rewrite the original text with the relation indices.

Task: process the following text:

Sam has a brother Mike. Suzy has a sister Sally. Suszy ahs a mother Jill, and a father Bill.
Mike is in San Diego. Sam is located is Oakland. Suzy's family all live in Los Angeles.
```

The ChatGPT 4 output is:

```
First, we create the indexed list of unique relationships:

brother, 1
located in, 2
sister, 1
sibling, 1
found at, 2
father, 1
location, 2
mother, 1
relative, 1

Now, we rewrite the original text using the relation indices:

Sam has a 1 Mike. Suzy has a 1 Sally. Suszy has a 1 Jill, and a 1 Bill.
Mike is 2 San Diego. Sam is 2 Oakland. Suzy's family all 2 Los Angeles.
```

In the first sentence **Sam has a 1 Mike** the number 1 refers to **brother**.

### Disambiguate Relationship Names and Rewrite Text, Only Output Rewritten Text

Here we output just results in JSON format.

Prompt:


```
We want to disambiguate relationship names and then rewrite text using canonical relationship names.

Given a list of relationships like:

brother
located in
sister
sibling
found at
father
location
mother
relative

Then generate a list with an index for each unique relationship, like:

brother, <1>
located in, <2>
sister, <1>
sibling, <1>
found at, <2>
father, <1>
location, <2>
mother, <1>
relative, <1>

And then rewrite the original text using the relation indices, and list relation names with indices. Only output rewritten text and names with indices.

Task: process the following text outputing JSON format:

Sam has a brother Mike. Suzy has a sister Sally. Suzy has a mother Jill, and a father Bill.
Mike is in San Diego. Sam is located is Oakland. Suzy's family all live in Los Angeles.
```

The ChatGPT 4 output is:

```json
{
  "relationships": {
    "brother": "<1>",
    "located in": "<2>",
    "sister": "<1>",
    "sibling": "<1>",
    "found at": "<2>",
    "father": "<1>",
    "location": "<2>",
    "mother": "<1>",
    "relative": "<1>"
  },
  "rewritten_text": "Sam has a brother Mike. Suzy has a sister Sally. Suzy has a mother Jill, and a father Bill.\nMike is <2> San Diego. Sam is <2> Oakland. Suzy's family all live <2> Los Angeles."
}
```

### Topic Modeling for Document Classification

In this example we show a complete template that includes document text. To use this in your programs, replace the document text with a token string that you can replace with the text for the document you are topic modeling.

Prompt:

```
Topic modeling involves reading text and assigning a topic name (also known as a document classification).
The topics that you know how to classify are:

sports
health
chemistry
economy
politics

The output format will be:

Known topics: <list of all topics>
Topic: <topic of text>

Task: what is the best topic name for the following text:

John and Sam went to the playing field yesterday. Sam sprained his ankle.
```

The result is:

```
Known topics: sports, health, chemistry, economy, politics

Topic: sports
```

In this prompt template, I used placeholder document text **John and Sam went to the playing field yesterday. Sam sprained his ankle.** that you would replace with the longer text from the document you want to determine the topic or classification of.

In this template, I like to ask the LLM to repeat the list of topics (or classifications) it knows.

## Prompt Engineering Wrapup

When you experiment with the examples in this chapter you can both use the ChatGPT web interface to modify and develop prompts and you can also revisit the chapter on OpenAI APIs and experiment with prompts programmatically. My personal preference is to perform initial experiments with the ChatGPT web interface but to do most of the iterative prompt development in Emacs with a SLIME REPL. 

I mentioned earlier Andrew Ng's and Isa Fulford's one hour prompt engineering class. The devote time and provide good advice on interactive developement. Their examples use the Python language.