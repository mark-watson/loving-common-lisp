# Common Lisp library to access local Ollama server APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.

2024/01/09: updated to latest ollama APIs, now using gpt-4

## Running the Ollama LLM service

    ollama serve

Install Ollama from [https://ollama.ai](https://ollama.ai)


## Example:

```lisp
cl-user> (ql:quickload :ollama)
cl-user> (ollama:completions "The President went to Congress to talk about the deficit and taxes")
It is not uncommon for a president to go to Congress to discuss matters related to the federal budget, including the deficit and taxes. During such speeches, the president typically outlines their proposals for addressing these issues, such as cuts to government spending, changes to tax policies, or other measures to reduce the deficit. These addresses often spark debate and discussion among members of Congress, who may have different views on how to address these complex challenges facing the country's economy.
```

Also try **ollama:summarize** and **ollama:answer-question**. See my book for examples and example output.


## Experiments with prompts (material for Prompt Engineering Chapter)

You should references both chapters on ollama APIs and Pronpt Engineering in my Common Lisp book that you can read online for free: [https://leanpub.com/lovinglisp/read](https://leanpub.com/lovinglisp/read)

### Experiment 1: find entities and relationships

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

### Experiment 2: disambiguate relationship names

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

### Experiment 3:  disambiguate relation ship names and rewrite text

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

### Experiment 4: disambiguate relation ship names and rewrite text, only output rewritten text

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

```
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

### Experiment 5: topic modeling (or document classification)

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

