# Find entities in text

From my book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:

````
(ql:quickload "entities")
(entities:make-entities-object "President Bill Clinton ran for president of the USA")
(entities:make-entities-object "President Bill Clinton ran for president of the USA. He campaigned on better public health care. Clinton was criticized for military actions in Yugoslavia and also for lying to Congress.")
````

