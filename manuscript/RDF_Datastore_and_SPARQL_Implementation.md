# Implementing a Simple RDF Datastore and Partial SPARQL Support in Common Lisp

This chapter explores a Common Lisp implementation of a basic RDF (Resource Description Framework) datastore with partial SPARQL (SPARQL Protocol and RDF Query Language) support. You can find the source code on GitHub [https://github.com/mark-watson/simple_rdf_sparql](https://github.com/mark-watson/simple_rdf_sparql).

Before we look at the implementation let’s look at code for an example use case and run it:

```lisp
(defun test ()
  (setf *rdf-store* nil)

  (add-triple "John" "age" "30")
  (add-triple "John" "likes" "pizza")
  (add-triple "Mary" "age" "25")
  (add-triple "Mary" "likes" "sushi")
  (add-triple "Bob" "age" "35")
  (add-triple "Bob" "likes" "burger")

  (print-all-triples)

  (defun print-query-results (query-string)
    (format t "Query: ~A~%" query-string)
    (let ((results (execute-sparql-query query-string)))
      (format t "Final Results:~%")
      (if results
          (dolist (result results)
            (format t "  ~{~A: ~A~^, ~}~%"
                    (loop for (var . value) in result
                          collect var collect value)))
          (format t "  No results~%"))
      (format t "~%")))

  (print-query-results "select * where { ?name age ?age . ?name likes ?food }")
  (print-query-results "select ?s ?o where { ?s likes ?o }")
  (print-query-results "select * where { ?name age ?age . ?name likes pizza }"))
```
  
Assuming that you cloned the repo into ~/quicklisp/local-projects, the library **simple_rdf_sparql** is available, for example:

```
 $ sbcl
* (ql:quickload :simple_rdf_sparql)
To load "simple_rdf_sparql":
  Load 1 ASDF system:
    simple_rdf_sparql
; Loading "simple_rdf_sparql"
[package simple_rdf_sparql]..
(:simple_rdf_sparql)
* (simple_rdf_sparql:test)
All triples in the datastore:
Bob likes burger
Bob age 35
Mary likes sushi
Mary age 25
John likes pizza
John age 30

Query: select * where { ?name age ?age . ?name likes ?food }
Final Results:
  ?age: 35, ?food: burger, ?name: Bob
  ?age: 25, ?food: sushi, ?name: Mary
  ?age: 30, ?food: pizza, ?name: John

Query: select ?s ?o where { ?s likes ?o }
Final Results:
  ?s: Bob, ?o: burger
  ?s: Mary, ?o: sushi
  ?s: John, ?o: pizza

Query: select * where { ?name age ?age . ?name likes pizza }
Final Results:
  ?age: 30, ?name: John

nil
* 
```

The GitHub repository [https://github.com/mark-watson/simple_rdf_sparql](https://github.com/mark-watson/simple_rdf_sparql) contains two project files **package.lisp** and **simple_rdf_sparql.asd** that I won’t list here.

We'll now break down the implementation code into several key components and explain each part step by step.

## 1. RDF Triple Structure

The foundation of our RDF datastore is the triple structure:

```lisp
(defstruct triple
  subject
  predicate
  object)
```

This structure represents the basic unit of data in RDF, consisting of a subject, predicate, and object. Each triple acts as a simple statement of fact, linking a subject to an object through a predicate. For example, consider the statement: "Mark authored the book." Here, "Mark" is the subject, "authored" is the predicate, and "book" is the object.

In RDF, triples allow us to represent complex relationships between entities in a highly structured manner, which can then be queried and analyzed. The subject typically represents the entity or resource being described, while the predicate denotes the type of relationship, and the object identifies the value or entity linked to the subject. These three components work together to form a directed graph, making the RDF model highly expressive for semantic web and linked data applications.

## 2. RDF Datastore

The datastore itself is a simple global variable:

```lisp
(defvar *rdf-store* nil)
```

This variable will hold a list of triples.

## 3. Basic Datastore Operations

Triples are stored in memory and two utility functions support creating new triples and deleting triples.

### Adding Triples

```lisp
(defun add-triple (subject predicate object)
  (push (make-triple :subject subject
                     :predicate predicate
                     :object object)
        *rdf-store*))
```

This function creates a new triple and adds it to the datastore.

### Removing Triples

```lisp
(defun remove-triple (subject predicate object)
  (setf *rdf-store*
        (remove-if (lambda (triple)
                     (and (equal (triple-subject triple) subject)
                          (equal (triple-predicate triple) predicate)
                          (equal (triple-object triple) object)))
                   *rdf-store*)))
```

This function removes a specific triple from the datastore.

## 4. Query Support

### Identifying Variables

```lisp
(defun variable-p (str)
  (and (stringp str) (> (length str) 0) (char= (char str 0) #\?)))
```

This helper function identifies if a string represents a variable (starting with '?').

### Converting Triples to Bindings

The function **triple-to-binding (triple &optional pattern)** is designed to convert a given RDF triple into a set of variable bindings based on a specified pattern. This is a crucial step in evaluating SPARQL queries like:

```sparql
select * where { ?name age ?age . ?name likes pizza }
```

In this context, the function matches variables in the SPARQL pattern against actual values in the datastore's triples. Let’s break down what the function does:

Function parameters:

- **triple**: This is the triple we want to match against. In RDF terms, a triple has three components: subject, predicate, and object.
- **pattern** (optional): The pattern here is a list of three elements, potentially containing variables (e.g., ?name, ?age).

A variable is represented as a symbol prefixed by ? in SPARQL, and the function uses function **variable-p** to check if an element in the pattern is a variable.

**Breakdown of the Function Logic**

The function starts by setting up an empty list called binding, which will store any matched variable-value pairs:

```lisp
(defun triple-to-binding (triple &optional pattern)
  (let ((binding nil))
```

Subject Matching in function **triple-to-binding**:

- The first when clause checks if the first element of the pattern is a variable ((variable-p (first pattern))).
- If true, it pairs the first variable in the pattern (?name in the example query) with the subject of the triple ((triple-subject triple)).

```lisp
  (when (and pattern (variable-p (first pattern)))
    (push (cons (first pattern)
                (triple-subject triple))
          binding))
```

This might generate a binding like **(?name . "Alice”)** if Alice is the subject in a matching triple.

Predicate Matching:

- Similarly, the second when clause in function **triple-to-binding** checks if the predicate in the pattern is a variable.
- If so, it pairs the second element of the pattern with the predicate value in the triple.

```lisp
  (when (and pattern (variable-p (second pattern)))
    (push (cons (second pattern)
                (triple-predicate triple))
          binding))
```

This will match patterns like **?relation** in **?name ?relation ?age***.

Object Matching:

- The final when clause in function **triple-to-binding** checks if the object in the pattern is a variable.
- If so, it pairs the third element of the pattern with the object value in the triple.

```lisp
  (when (and pattern (variable-p (third pattern)))
    (push (cons (third pattern)
                (triple-object triple))
          binding))
```

For example, this would produce a binding like **(?age . 25)** if a triple has 25 as the object and the third predicate in the matching list was **?age**.

Now, return the binding as the returned value for a function call to function **triple-to-binding**: after processing the pattern, the function returns the generated bindings as a list of **(variable . value)** pairs:

```lisp
  binding))
```

This function converts a triple to a binding, matching variables in the pattern to values in the triple.

If we call **triple-to-binding** with this triple and the pattern **( ?name age ?age )**, the function performs the following steps:

Checks if **?name** is a variable (using function **variable-p**), which if true, then creates a binding for **?name** as:

    (?name . "Alice")

and bind **?age**, for example:

    (?age 25) 

**How This Fits into SPARQL Processing**

In the larger scope of evaluating a SPARQL query:

- Pattern Matching: **triple-to-binding** is applied to match patterns like **{ ?name age ?age }** against all triples in the datastore.
- Generating Bindings: Each matching triple generates a set of bindings for the variables in the query.
- Query Evaluation: These bindings are used to check additional conditions (like **{ ?name likes pizza }**) and produce the final results for the select * query.

### Querying Triples

The **query-triples** function performs a search over an RDF datastore (*rdf-store*) to find triples that match the given subject, predicate, and object parameters. It iterates through all triples stored in the datastore and uses function **remove-if-not** to filter out those that do not match the specified criteria. For each triple, the function **query-triples** evaluates whether its subject, predicate, and object match the respective parameters provided in the function call.

Specifically, function **query-triples** checks each parameter using a combination of and and or conditions: if a parameter is nil, it is treated as a wildcard and will match any value; if the parameter is a variable (identified by the **variable-p** function), it is considered to match any value as well; otherwise, the parameter must be equal to the corresponding component of the triple (checked using **equal**). If a triple satisfies all three conditions, either matching the specified values or variables, it is kept in the result set; otherwise, it is filtered out. The function ultimately returns a list of triples from *rdf-store* that match the given subject, predicate, and object pattern, enabling flexible querying capabilities over the RDF datastore:

```lisp
(defun query-triples (subject predicate object)
  (remove-if-not
    (lambda (triple)
       (and (or (null subject)
                (variable-p subject)
                (equal (triple-subject triple) subject))
            (or (null predicate)
                (variable-p predicate)
                (equal (triple-predicate triple) predicate))
            (or (null object)
                (variable-p object)
                (equal (triple-object triple) object))))
     *rdf-store*))
```

This function queries the datastore for triples matching the given pattern.

## 5. SPARQL Query Structure

```lisp
(defstruct sparql-query
  select-vars
  where-patterns)
```

This structure represents a simplified SPARQL query with select variables and where patterns. Please note that this **defstruct** creates the function **make-sparql-query** that we use in the next section.

## 6. SPARQL Query Parsing

The **parse-sparql-query** function takes a SPARQL query string as input and processes it to extract its main components: the variables to be selected and the patterns in the WHERE clause. It starts by splitting the query string into individual tokens and removes curly braces ({ and }), which are not needed for the parsing. It then identifies the positions of the "select" and "where" keywords within the tokenized list using position. Using these indices, the function extracts the variables listed after the "select" keyword up to the "where" keyword, storing them in the **select-vars** variable. Next, it extracts the tokens following the "where" keyword, storing them as where-clause. This where-clause is further processed using a helper function, **parse-where-patterns**, which converts the clause into a structured list of patterns that represent the triple patterns in the query. Finally, the function returns a new **sparql-query** object (constructed using **make-sparql-query**) that encapsulates both the extracted variables and parsed patterns, making it suitable for further evaluation against an RDF datastore:

```lisp
(defun parse-sparql-query (query-string)
  (let* ((tokens
          (remove-if
            (lambda (token)
              (member token '("{" "}") :test #'string=))
            (split-string query-string)))
         (select-index (position "select" tokens :test #'string-equal))
         (where-index (position "where" tokens :test #'string-equal))
         (select-vars (subseq tokens (1+ select-index) where-index))
         (where-clause (subseq tokens (1+ where-index)))
         (where-patterns (parse-where-patterns where-clause)))
    (make-sparql-query :select-vars select-vars
                       :where-patterns where-patterns)))
```

This function parses a SPARQL query string into a structured representation.

## 7. Query Execution

The **execute-where-patterns** function recursively evaluates a list of SPARQL WHERE patterns against an RDF datastore, returning a list of variable bindings that satisfy the entire pattern sequence. If the patterns list is empty, it returns a list containing an empty binding ((list nil)), indicating that no more patterns need to be matched. Otherwise, it starts by extracting the first pattern and the remaining patterns, and uses **query-triples** to find triples in the RDF datastore that match the first pattern. It then generates a set of initial bindings for each matching triple using the **triple-to-binding** function. If there are no more patterns left to evaluate, it returns these bindings as the final results. If there are remaining patterns, it recursively processes the rest of the patterns by invoking **execute-where-patterns-with-bindings**, using each binding to constrain the search for subsequent patterns. For each binding in the current level, it combines the results of the recursive call using **mapcan** and merges the bindings (merge-bindings) to form a unified set of results that meets all the patterns in sequence, thereby producing a comprehensive solution for the entire WHERE clause:


```lisp
(defun execute-where-patterns (patterns)
  (if (null patterns)
      (list nil)
      (let* ((pattern (first patterns))
             (remaining-patterns (rest patterns))
             (matching-triples (apply #'query-triples pattern)))
        (let ((bindings
               (mapcar
                 (lambda (triple)
                   (triple-to-binding triple pattern))
                 matching-triples)))
          (if (null remaining-patterns)
              bindings
              (mapcan
                (lambda (binding)
                  (let ((results
                         (execute-where-patterns-with-bindings
                            remaining-patterns binding)))
                    (mapcar (lambda (result)
                              (merge-bindings binding result))
                            results)))
                      bindings))))))
```

This function executes the WHERE patterns of a SPARQL query, finding matching triples and generating bindings.

## 8. Result Projection

The **project-results** function processes a list of query results to filter or project only the variables specified in the **select-vars** list. If **select-vars** is set to "*", indicating that all variables should be included, it simply returns the results after removing any duplicate bindings. Otherwise, it iterates over each result, extracts only the variables listed in **select-vars**, and constructs a new binding list for each result that includes these selected variables and their corresponding values. Afterward, it removes any duplicate bindings in the filtered results and returns the final projected set, ensuring that only the desired variables are included in the output:

```lisp
(defun project-results (results select-vars)
  (if (equal select-vars '("*"))
      (mapcar #'remove-duplicate-bindings results)
      (mapcar (lambda (result)
                (remove-duplicate-bindings
                 (mapcar (lambda (var)
                           (cons var (cdr (assoc var result :test #'string=))))
                         select-vars)))
              results)))
```

This function projects the query results based on the SELECT variables.

## 9. Main Query Execution

The **execute-sparql-query** function orchestrates the entire process of executing a SPARQL query by integrating various helper functions to parse, evaluate, and project the query results. It starts by parsing the input query string using **parse-sparql-query**, extracting the WHERE patterns and SELECT variables from the parsed query structure. It then uses **execute-where-patterns** to evaluate the WHERE clause against the RDF datastore, generating a list of variable bindings that satisfy the given patterns. After obtaining these intermediate results, it applies **project-results** to filter and project only the variables specified in the SELECT clause, resulting in the final output. This function effectively takes a complete SPARQL query, processes it step-by-step, and returns the matching results in the desired format:


```lisp
(defun execute-sparql-query (query-string)
  (let* ((query (parse-sparql-query query-string))
         (where-patterns (sparql-query-where-patterns query))
         (select-vars (sparql-query-select-vars query))
         (results (execute-where-patterns where-patterns))
         (projected-results (project-results results select-vars)))
    projected-results))
```

This function ties everything together, executing a SPARQL query from start to finish.

## Conclusion

This implementation provides a basic framework for an RDF datastore with partial SPARQL support in Common Lisp. While it lacks many features of a full-fledged RDF database and SPARQL engine, it demonstrates the core concepts and can serve as a starting point for more complex implementations. The code showcases Common Lisp's strengths in list processing and symbolic computation, making it well-suited for working with semantic data structures like RDF.