# Using the Microsoft Bing Search APIs

I have used the Bing search APIs for many years. Microsoft Bing supports several commercial search engine services, including my favorite search engine Duck Duck Go. Bing is now part of the Azure infrastructure that is branded as "Cognitive Services." You should find the example code for this chapter relatively easy to extend to other Azure Cognitive Services that you might need to use.

You will need to register with Microsoft's Azure search service to use the material in this chapter. It is likely that you view search as a manual human-centered activity. I hope to expand your thinking to considering applications that automate search, finding information on the web, and automatically organizing information.

While the example code uses only the search APIs, with some modification it can be extended to work with all REST APIs provided by [Azure Cognitive Services](https://azure.microsoft.com/en-us/services/cognitive-services/) that include: analyzing text to get user intent, general language understanding, detecting key phrases and entity names, translate between languages, converting between speech and text, and various computer vision services. These services are generally free or very low cost for a few thousand API calls a month, with increased cost for production deployments. Microsoft spends about $1 billion a year in research and development for Azure Cognitive Services.

## Getting an Access Key for Microsoft Bing Search APIs

You will need to set up an Azure account if you don't already have one. I use the Bing search APIs fairly often for research but I have never spent more than about a dollar a month and usually I get no bill at all. For personal use it is a very inexpensive service.

You start by going to the web page [https://azure.microsoft.com/en-us/try/cognitive-services/](https://azure.microsoft.com/en-us/try/cognitive-services/) and sign up for an access key. The Search APIs sign up is currently in the fourth tab in this web form. When you navigate to the Search APIs tab, select the option Bing Search APIs v7. You will get an API key that you need to store in an environment variable that you will soon need:

{lang="bash",linenos=off}
~~~~~~~~
export BING_SEARCH_V7_SUBSCRIPTION_KEY=1e97834341d2291191c772b7371ad5b7
~~~~~~~~


That is not my real subscription key!

You also set the Bing search API as an environment variable:

{lang="bash",linenos=off}
~~~~~~~~
export BING_SEARCH_V7_ENDPOINT=https://api.cognitive.microsoft.com/bing/v7.0/search
~~~~~~~~


## Example Search Script

Instead of using a pure Common Lisp HTTP client library I often prefer using the **curl** command run in a separate process. The **curl** utility handles all possible authentication modes, handles headers, response data in several formats, etc. We capture the output from **curl** in a string that in turn gets processed by a JSON library.

It takes very little Common Lisp code to access the Bing search APIs. The function **websearch** makes a generic web search query. The function **get-wikidata-uri** uses the **websearch** function by adding "site:wikidata.org" to the query and returning only the WikiData URI for the original search term. We will later see several examples. I will list the entire library with comments to follow:

{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:bing)

(defun get-wikidata-uri (query)
  (let ((sr (websearch (concatenate 'string "site:wikidata.org " query))))
    (cadar sr)))

(defun websearch (query)
  (let* ((key (uiop:getenv "BING_SEARCH_V7_SUBSCRIPTION_KEY"))
         (endpoint (uiop:getenv "BING_SEARCH_V7_ENDPOINT"))
         (command
          (concatenate
	   'string
	   "curl -v -X GET \""  endpoint "?q="
	   (drakma:url-encode query :utf-8)
	   "&mkt=en-US&limit=4\""
	   " -H \"Ocp-Apim-Subscription-Key: " key "\""))
         (response
          (uiop:run-program command :output :string)))
    (with-input-from-string
	(s response)
      (let* ((json-as-list (json:decode-json s))
             (values (cdadr (cddr (nth 2 json-as-list)))))
	(mapcar #'(lambda (x)
                    (let ((name (assoc :name x))
			  (display-uri (assoc :display-url x))
			  (snippet (assoc :snippet x)))
		      (list (cdr name) (cdr display-uri) (cdr snippet))))
		values)))))
~~~~~~~~

We get the Bing access key and the search API endpoint in lines 8-9. Lines 10-16 create a complete call to the **curl* command line utility. We spawn a process to run **curl** and capture the string output in the variable **response** in lines 17-18. You might want to add a few print statements to see typical values for the variables **command** and **response**. The response data is JSON data encoded in a string, with straightforward code in lines 19-28 to parse out the values we want.

The following repl listing shows this library in use:

{lang="bash",linenos=off}
~~~~~~~~
$ sbcl
This is SBCL 2.0.2, an implementation of ANSI Common Lisp.
* (ql:quickload "bing")
To load "bing":
  Load 1 ASDF system:
    bing
; Loading "bing"
..............
("bing")
* (bing:get-wikidata-uri "Sedona Arizona")
"https://www.wikidata.org/wiki/Q80041"
* (bing:websearch "Berlin")
(("Berlin - Wikipedia" "https://en.wikipedia.org/wiki/Berlin"
  "Berlin (/ bɜːrˈlɪn /; German: [bɛʁˈliːn] (listen)) is the capital and largest city of Germany by both area and population. Its 3,769,495 (2019) inhabitants make it the most populous city proper of the European Union. The city is one of Germany's 16 federal states.")
 ("THE 15 BEST Things to Do in Berlin - 2020 (with Photos ..."
  "https://www.tripadvisor.com/Attractions-g187323-Activities-Berlin.html"
  "Book your tickets online for the top things to do in Berlin, Germany on Tripadvisor: See 571,599 traveler reviews and photos of Berlin tourist attractions. Find what to do today, this weekend, or in August. We have reviews of the best places to see in Berlin. Visit top-rated & must-see attractions.")
 ("Berlin - Official Website of the City of Berlin, Capital ..."
  "https://www.berlin.de/en"
  "Official Website of Berlin: Information about the Administration, Events, Culture, Tourism, Hotels and Hotel Booking, Entertainment, Tickets, Public Transport, Political System, Local Authorities and Business in Berlin.")
 ("Berlin | History, Map, Population, Attractions, & Facts ..."
  "https://www.britannica.com/place/Berlin"
  "Berlin is situated about 112 miles (180 km) south of the Baltic Sea, 118 miles (190 km) north of the Czech-German border, 110 miles (177 km) east of the former inner-German border, and 55 miles (89 km) west of Poland. It lies in the wide glacial valley of the Spree River, which runs through the centre of the city.")
 ("Berlin travel | Germany - Lonely Planet"
  "https://www.lonelyplanet.com/germany/berlin"
  "Welcome to Berlin Berlin's combo of glamour and grit is bound to mesmerise all those keen to explore its vibrant culture, cutting-edge architecture, fabulous food, intense parties and tangible history.")
 ("Berlin 2020: Best of Berlin, Germany Tourism - Tripadvisor"
  "https://www.tripadvisor.com/Tourism-g187323"
  "Berlin is an edgy city, from its fashion to its architecture to its charged political history. The Berlin Wall is a sobering reminder of the hyper-charged postwar atmosphere, and yet the graffiti art that now covers its remnants has become symbolic of social progress.")
 ("Berlin 2020: Best of Berlin, OH Tourism - Tripadvisor"
  "https://www.tripadvisor.com/Tourism-g50087-Berlin_Ohio-Vacations.html"
  "Berlin Tourism: Tripadvisor has 11,137 reviews of Berlin Hotels, Attractions, and Restaurants making it your best Berlin resource.")
 ("Berlin (band) - Wikipedia" "https://en.wikipedia.org/wiki/Berlin_(band)"
  "Berlin is the alias for vocalist Terri Nunn, as well as the American new wave band she fronts, having been originally formed in Orange County, California. The band gained mainstream-commercial success with singles including \" Sex (I'm A...) \", \" No More Words \" and the chart-topping \" Take My Breath Away \" from the 1986 film Top Gun.")
 ("Berlin's official travel website - visitBerlin.de"
  "https://www.visitberlin.de/en"
  "Berlin's way to a metropolis 100 Years of Greater Berlin In 1920, modern Berlin was born at one fell swoop. 8 cities, 59 rural communities and 27 manor districts unite to form \"Greater Berlin\""))
* 
~~~~~~~~

I have been using the Bing search APIs for many years. They are a standard part of my application building toolkit.


## Wrap-up

You can check out the wide range of [Congitive Services](https://azure.microsoft.com/en-us/try/cognitive-services/) on the Azure site. Available APIs include: language detection, speech recognition, vision libraries for object recognition, web search, and anomaly detection in data.

In addition to using automated web scraping to get data for my personal research, I often use automated web search. I find the Microsoft's Azure Bing search APIs are the most convenient to use and I like paying for services that I use.
