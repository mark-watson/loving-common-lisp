# Wolfram Language experiments

In order to handle embeded strings in commands sent to Wolfram Language, I like to use single quotes, and then replace them. Original command:

```
TextCases["NYC, Los Angeles, and Chicago are the largest cities in the USA in 2018 according to Pete Wilson.",
   {"City", "Country", "Date", "Person"} -> {"String", "Interpretation", "Probability"}]
```

```lisp
(setf example "TextCases['NYC, Los Angeles, and Chicago are the largest cities in the USA in 2018 according to Pete Wilson.', {'City', 'Country', 'Date', 'Person'} -> {'String', 'Interpretation', 'Probability'}]")
(setf example-str (myutils:replace-all  example "'" "\""))
(setf results (wolfram:wolfram example-str))
(pprint results)
```

This produces:

```
("Association"
 ("Rule"
  "'City'"
  ("List"
   ("List" "'NYC'" ("Entity" "'City'" ("List" "'NewYork'" "'NewYork'" "'UnitedStates'")) 0.76187855)
   ("List" "'Los Angeles'" ("Entity" "'City'" ("List" "'LosAngeles'" "'California'" "'UnitedStates'")) 0.8419536)
   ("List" "'Chicago'" ("Entity" "'City'" ("List" "'Chicago'" "'Illinois'" "'UnitedStates'")) 0.9131763)))
 ("Rule" "'Country'" ("List" ("List" "'USA'" ("Entity" "'Country'" "'UnitedStates'") 0.9304722)))
 ("Rule" "'Date'" ("List" ("List" "'2018'" ("DateObject" ("List" 2018) "'Year'" "'Gregorian'" -7.0) 0.83740867)))
 ("Rule" "'Person'" ("List" ("List" "'Pete Wilson'" ("Entity" "'Person'" "'PeteWilson::s7259'") 0.9274548))))
```

or:

```lisp
(setf results (wolfram:cleanup-lists (wolfram:wolfram example-str)))
(pprint results)
```

```
("Association"
 ("'City'"
  (("'NYC'" ("Entity" "'City'" ("'NewYork'" "'NewYork'" "'UnitedStates'")) 0.75583166)
   ("'Los Angeles'" ("Entity" "'City'" ("'LosAngeles'" "'California'" "'UnitedStates'")) 0.84206486)
   ("'Chicago'" ("Entity" "'City'" ("'Chicago'" "'Illinois'" "'UnitedStates'")) 0.91092855)))
 ("'Country'" (("'USA'" ("Entity" "'Country'" "'UnitedStates'") 0.9285077)))
 ("'Date'" (("'2018'" ("DateObject" (2018) "'Year'" "'Gregorian'" -7.0) 0.8364356)))
 ("'Person'" (("'Pete Wilson'" ("Entity" "'Person'" "'PeteWilson::s7259'") 0.9274548))))
```


