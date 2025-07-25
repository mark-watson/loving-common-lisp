# Resolve entities in text to DBPedia URIs for the discovered entities

From my book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:


````

(ql:quickload "entity-uris")

(entity-uris:pp-entities "Bill Clinton and George Bush went to Mexico and England and watched Univision. They enjoyed Dakbayan sa Dabaw and shoped at Best Buy and listened to Al Stewart. They agree on República de Nicaragua and support Sweden Democrats and Leicestershire Miners Association and both sent their kids to Darul Uloom Deoband.")

````
