This is the stub README.txt for the "kbnlp" project.

try:

````

(ql:quickload "entities_dbpedia")

(setf
  an-entity-hash
   (entities_dbpedia:find-entities-in-text "Bill Clinton and George Bush went to Mexico and England and watched Univision. They enjoyed Dakbayan sa Dabaw and shoped at Best Buy and listened to Al Stewart. They agree on República de Nicaragua and support Sweden Democrats and Leicestershire Miners Association and both sent their kids to Darul Uloom Deoband."))

(entities_dbpedia:entity-iterator
  #'(lambda (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))
  an-entity-hash)
 
````
