# Knowledge Graph Navigator Text-Based User Interface {#kgntext}

We developed the The Knowledge Graph Navigator (which I will often refer to as KGN) common library in the last chapter. Here we write a simple console or text-based user interface for the library. In later chapters we implement UIs using LispWorks CAPI, McCLIM, and Franz Common Graphics.

This Quicklisp library can be found in a separate GitHub repository [https://github.com/mark-watson/kgn-text-ui](https://github.com/mark-watson/kgn-text-ui) and contains the files:

- kgn-text-ui.asd - specifies dependencies, including the KGN common library
- kgn-text-ui.lisp - Contains the complete user interface
- package.lisp - specifies dependencies, including the KGN common library

We start by looking at sample output using the text user interface and then look at the implementation.

## Example Output

We will look at a very simple example query **Bill Gates worked at Microsoft and his competitor was IBM** that only contains a few entities. In practice, I usually use queries with five to ten entities to get more discovered relationships. I remove a lot of the generated output in the following listing for brevity, especially the many generated SPARQL queries that the code generates and uses (comments on the output appear after this listing):

{linenos=on}
~~~~~~~~
$ sbcl
*(ql:quickload :kgn-text-ui)
; Loading "kgn-common"
; Loading "sqlite"
; Loading "cl-json"
; Loading "drakma"

* (kgn-text-ui:kgn-text-ui)

"Enter entity names (people, places, companies, etc.":
Bill Gates worked at Microsoft and his competitor was IBM

- - - - Enter zero or more indices for your desired selections:

0  -   "William Henry Gates III (born October 28, 1955) is an American business magnate, software developer, investor, author, and philanthropist. He is a co-founder of Microsoft, along with his late childhood friend Paul Allen. During his career at Microsoft, Gates held the positions of chairman, chief executive officer (CEO), president and chief software architect, while also being the largest individual shareholder until May 2014. He is considered one of the best known entrepreneurs of the microcomputer revolution of the 1970s and 1980s." 

1  -   "Harry Roy Lewis (born 1947) is an American computer scientist, mathe 00ADma 00ADti 00ADcian, and uni 00ADver 00ADsity admin 00ADi 00ADstra 00ADtor known for his research in com 00ADpu 00ADta 00ADtional logic, textbooks in theoretical computer science, and writings on computing, higher education, and technology. He is Gordon McKay Professor of Computer Science at Harvard University, and was Dean of Harvard College from 1995 to 2003. A new professorship in Engineering and Applied Sciences, endowed by a former student, will be named for Lewis and his wife upon their retirements." 

2  -   "Cascade Investment, L.L.C. is an American holding company and private investment firm headquartered in Kirkland, Washington, United States. It is controlled by Bill Gates, and managed by Michael Larson. More than half of Gates' fortune is held in assets outside his holding of Microsoft shares. Cascade is the successor company to Dominion Income Management, the former investment vehicle for Gates' holdings, which was managed by convicted felon Andrew Evans." 

3  -   "Jerry P. Dyer (born May 3, 1959) is an American politician and former law enforcement officer. He is the 26th and current mayor of Fresno, California. Previously, he served as the chief of the Fresno Police Department." 

0

- - - - Enter zero or more indices for your desired selections:

0  -   "Kenexa, an IBM Company, provides employment and retention services. This includes recruitment process outsourcing onboarding tools, employee assessment, abilities assessment for employment candidates (Kenexa Prove It); and Kenexa Interview Builder, a structured interview archive with example questions." 

1  -   "Sequent Computer Systems was a computer company that designed and manufactured multiprocessing computer systems. They were among the pioneers in high-performance symmetric multiprocessing (SMP) open systems, innovating in both hardware (e.g., cache management and interrupt handling) and software (e.g., read-copy-update). Vestiges of Sequent's innovations live on in the form of data clustering software from PolyServe (subsequently acquired by HP), various projects within OSDL, IBM contributions to the Linux kernel, and claims in the SCO v. IBM lawsuit." 

2  -   "i2 Limited was the UK-based arm of software company i2 Group which produced visual intelligence and investigative analysis software for military intelligence, law enforcement and commercial agencies. After a number of acquisitions, in 2011 it became part of IBM." 

3  -   "The International Technology Alliance in Distributed Analytics and Information Sciences (DAIS-ITA) is a research program initiated by the UK Ministry of Defence (United Kingdom) (MOD) and the US Army Research Laboratory (ARL), in September 2016. It is led by IBM Research in the U.S. and IBM Hursley in the UK. DAIS ITA is the second International Technology Alliance started by the two countries, succeeding the previous ten year alliance NIS-ITA, which was of similar nature." 

4  -   "The International Technology Alliance in Network and Information Sciences (NIS-ITA) was a research program initiated by the UK Ministry of Defence (United Kingdom) (MoD) and the US Army Research Laboratory (ARL), which was active for 10 years from May 2006 to May 2016. It was led by IBM Research in the U.S. and IBM Hursley in the UK. NIS ITA was the first International Technology Alliance started by the two countries." 

5  -   "Applix Inc. was a computer software company founded in 1983 based in Westborough, Massachusetts that published Applix TM1, a multi-dimensional online analytical processing (MOLAP) database server, and related presentation tools, including Applix Web and Applix Executive Viewer. Together, Applix TM1, Applix Web and Applix Executive Viewer were the three core components of the Applix Business Analytics Platform. (Executive Viewer was subsequently discontinued by IBM.)" 

6  -   "Ounce Labs (an IBM company) is a Waltham, Massachusetts-based security software vendor. The company was founded in 2002 and created a software analysis product that analyzes source code to identify and remove security vulnerabilities. The security software looks for a range of vulnerabilities that leave an application open to attack. Customers have included GMAC, Lockheed Martin, and the U.S. Navy. On July 28, 2009, Ounce was acquired by IBM, for an undisclosed sum, with the intention of integrating it into IBM's Rational Software business." 

7  -   "IBM Watson Health is a digital tool that helps clients facilitate medical research, clinical research, and healthcare solutions, through the use of artificial intelligence, data, analytics, cloud computing, and other advanced information technology. It is a division of the International Business Machines Corporation, (IBM), an American multinational information technology company headquartered in Armonk, New York." 

8  -   "International Business Machines Corporation (IBM) is an American multinational technology corporation headquartered in Armonk, New York, with operations in over 171 countries. The company began in 1911, founded in Endicott, New York by trust businessman Charles Ranlett Flint, as the Computing-Tabulating-Recording Company (CTR) and was renamed \"International Business Machines\" in 1924. IBM is incorporated in New York." 

9  -   "Microsoft Corporation is an American multinational technology corporation which produces computer software, consumer electronics, personal computers, and related services. Its best known software products are the Microsoft Windows line of operating systems, the Microsoft Office suite, and the Internet Explorer and Edge web browsers. Its flagship hardware products are the Xbox video game consoles and the Microsoft Surface lineup of touchscreen personal computers. Microsoft ranked No. 21 in the 2020 Fortune 500 rankings of the largest United States corporations by total revenue; it was the world's largest software maker by revenue as of 2016. It is considered one of the Big Five companies in the U.S. information technology industry, along with Amazon, Google (Alphabet), Apple, and Facebook (" 

10  -   "The CSS Working Group (Cascading Style Sheets Working Group) is a working group created by the World Wide Web Consortium (W3C) in 1997, to tackle issues that had not been addressed with CSS level 1. As of December 2019, the CSSWG had 142 members. The working group is co-chaired by and ." 

11  -   "The AMD Professional Gamers League (PGL), founded around 1997, was one of the first professional computer gaming eSports leagues. The PGL was run by Total Entertainment Network and was sponsored by AMD. The first professional tournament they held was for StarCraft in September 1997. The league was official unveiled at a press conference at Candlestick Park in San Francisco on November 3, 1997. It was sponsored by Microsoft, Nvidia, and Levi Strauss & Co. The organization raised over $1.2mil USD in sponsorship money." 

12  -   "Secure Islands Technologies Ltd. was an Israeli privately held technology company headquartered in Beit Dagan which was subsequently acquired by Microsoft. The company develops and markets Information Protection and Control (IPC) solutions." 

13  -   "Microsoft Innovation Centers (MICs) are local government organizations, universities, industry organizations, or software or hardware vendors who partner with Microsoft with a common goal to foster the growth of local software economies. These are state of the art technology facilities which are open to students, developers, IT professionals, entrepreneurs, startups and academic researchers. While each Center tunes its programs to local needs, they all provide similar content and services designed to accelerate technology advances and stimulate local software economies through skills and professional training, industry partnerships and innovation. As of 10 September 2010, there are 115 Microsoft Innovation Centers worldwide, most of which are open to the public. Recently it was reported th" 

14  -   "Press Play ApS was a Danish video game development studio based in central Copenhagen in Denmark. Since 2006, Press Play have released five titles, including the Max & the Magic Marker, Max: The Curse of Brotherhood and Kalimba. On November 10, 2016, Flashbulb acquired Press Play and its library of games to republish under the Flashbulb name including Kalimba, Tentacles: Enter the Mind, and Max: The Curse of Brotherhood." 

8 9

- - - ENTITY TYPE: people - - -

SPARQL to get PERSON data for <http://dbpedia.org/resource/Bill_Gates>:

"SELECT DISTINCT ?label ?comment@@ (GROUP_CONCAT (DISTINCT ?birthplace; SEPARATOR=' | ') AS ?birthplace) @@ (GROUP_CONCAT (DISTINCT ?almamater; SEPARATOR=' | ') AS ?almamater) @@ (GROUP_CONCAT (DISTINCT ?spouse; SEPARATOR=' | ') AS ?spouse) { @@ <http://dbpedia.org/resource/Bill_Gates> <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') . @@ OPTIONAL { <http://dbpedia.org/resource/Bill_Gates> <http://dbpedia.org/ontology/birthPlace> ?birthplace } . @@ OPTIONAL { <http://dbpedia.org/resource/Bill_Gates> <http://dbpedia.org/ontology/almaMater> ?almamater } . @@ OPTIONAL { <http://dbpedia.org/resource/Bill_Gates> <http://dbpedia.org/ontology/spouse> ?spouse } . @@ OPTIONAL { <http://dbpedia.org/resource/Bill_Gates>  <http://www.w3.org/2000/01/rdf-schema#label> ?label .@@ FILTER  (lang(?label) = 'en') } @@ } LIMIT 10@@"


label: Bill Gates

comment: William Henry Gates III (born October 28, 1955) is an American business magnate, software developer, investor, author, and philanthropist. He is a co-founder of Microsoft, along with his late childhood friend Paul Allen. During his career at Microsoft, Gates held the positions of chairman, chief executive officer (CEO), president and chief software architect, while also being the largest individual shareholder until May 2014. He is considered one of the best known entrepreneurs of the microcomputer revolution of the 1970s and 1980s.

birthplace: http://dbpedia.org/resource/Seattle | http://dbpedia.org/resource/Washington_(state)

almamater: 

spouse: http://dbpedia.org/resource/Melinda_French_Gates

- - - ENTITY TYPE: companies - - -

SPARQL to get COMPANY data for <http://dbpedia.org/resource/IBM>:


"SELECT DISTINCT ?label ?comment (GROUP_CONCAT (DISTINCT ?industry; SEPARATOR=' | ') AS ?industry)@@ (GROUP_CONCAT (DISTINCT ?netIncome; SEPARATOR=' | ') AS ?netIncome)@@ (GROUP_CONCAT (DISTINCT ?numberOfEmployees; SEPARATOR=' | ') AS ?numberOfEmployees) {@@ <http://dbpedia.org/resource/IBM> <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') .@@ OPTIONAL { <http://dbpedia.org/resource/IBM> <http://dbpedia.org/ontology/industry> ?industry } .@@  OPTIONAL { <http://dbpedia.org/resource/IBM> <http://dbpedia.org/ontology/netIncome> ?netIncome } .@@  OPTIONAL { <http://dbpedia.org/resource/IBM> <http://dbpedia.org/ontology/numberOfEmployees> ?numberOfEmployees } .@@  OPTIONAL { <http://dbpedia.org/resource/IBM> <http://www.w3.org/2000/01/rdf-schema#label> ?label . FILTER (lang(?label) = 'en') } @@ } LIMIT 30@@"


label: IBM

comment: International Business Machines Corporation (IBM) is an American multinational technology corporation headquartered in Armonk, New York, with operations in over 171 countries. The company began in 1911, founded in Endicott, New York by trust businessman Charles Ranlett Flint, as the Computing-Tabulating-Recording Company (CTR) and was renamed "International Business Machines" in 1924. IBM is incorporated in New York.

industry: http://dbpedia.org/resource/Artificial_intelligence | http://dbpedia.org/resource/Automation | http://dbpedia.org/resource/Blockchain | http://dbpedia.org/resource/Cloud_computing | http://dbpedia.org/resource/Computer_hardware | http://dbpedia.org/resource/Quantum_computing | http://dbpedia.org/resource/Robotics | http://dbpedia.org/resource/Software

net-income: 5.59E9

number-of-employees: 345900

SPARQL to get COMPANY data for <http://dbpedia.org/resource/Microsoft>:


"SELECT DISTINCT ?label ?comment (GROUP_CONCAT (DISTINCT ?industry; SEPARATOR=' | ') AS ?industry)@@ (GROUP_CONCAT (DISTINCT ?netIncome; SEPARATOR=' | ') AS ?netIncome)@@ (GROUP_CONCAT (DISTINCT ?numberOfEmployees; SEPARATOR=' | ') AS ?numberOfEmployees) {@@ <http://dbpedia.org/resource/Microsoft> <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .@@
                           FILTER  (lang(?comment) = 'en') .@@ OPTIONAL { <http://dbpedia.org/resource/Microsoft> <http://dbpedia.org/ontology/industry> ?industry } .@@  OPTIONAL { <http://dbpedia.org/resource/Microsoft> <http://dbpedia.org/ontology/netIncome> ?netIncome } .@@  OPTIONAL { <http://dbpedia.org/resource/Microsoft> <http://dbpedia.org/ontology/numberOfEmployees> ?numberOfEmployees } .@@  OPTIONAL { <http://dbpedia.org/resource/Microsoft> <http://www.w3.org/2000/01/rdf-schema#label> ?label . FILTER (lang(?label) = 'en') } @@ } LIMIT 30@@"


label: Microsoft

comment: Microsoft Corporation is an American multinational technology corporation which produces computer software, consumer electronics, personal computers, and related services. Its best known software products are the Microsoft Windows line of operating systems, the Microsoft Office suite, and the Internet Explorer and Edge web browsers. Its flagship hardware products are the Xbox video game consoles and the Microsoft Surface lineup of touchscreen personal computers. Microsoft ranked No. 21 in the 2020 Fortune 500 rankings of the largest United States corporations by total revenue; it was the world's largest software maker by revenue as of 2016. It is considered one of the Big Five companies in the U.S. information technology industry, along with Amazon, Google (Alphabet), Apple, and Facebook (

industry: http://dbpedia.org/resource/Cloud_computing | http://dbpedia.org/resource/Computer_hardware | http://dbpedia.org/resource/Consumer_electronics | http://dbpedia.org/resource/Corporate_venture_capital | http://dbpedia.org/resource/Internet | http://dbpedia.org/resource/Social_networking_service | http://dbpedia.org/resource/Software_development | http://dbpedia.org/resource/Video_game_industry

net-income: 6.06E10

number-of-employees: 182268

DISCOVERED RELATIONSHIP LINKS:

  <http://dbpedia.org/resource/Bill_Gates>   
    <http://dbpedia.org/ontology/knownFor>     
    <http://dbpedia.org/resource/Microsoft> .


  <http://dbpedia.org/resource/Microsoft>    
    <http://dbpedia.org/ontology/foundedBy>    
    <http://dbpedia.org/resource/Bill_Gates> .


  <http://dbpedia.org/resource/Microsoft>    
    <http://dbpedia.org/property/founders>     
    <http://dbpedia.org/resource/Bill_Gates> .

"Enter entity names (people, places, companies, etc.":

~~~~~~~~

On line 10 I input a test phrase "Bill Gates worked at Microsoft and his competitor was IBM." In lines 13-41 the test program prints out matching human entities from DBPedia that are indexed starting at 0. On line 43 I entered 0 to choose just the first entity "William Henry Gates III".

The prompt on line 45 asks the user to enter the indices for the company DBPedia entities they want to use. These companies are listed in lines 47-152. On line 154 I entered "8 9" to select two entities to use.

Lines 156-171 show the automatically generated SPARQL query to get information about Bill Gates. This information is printed on lines 174-189. I list more generated SPARQL queries and results (which we will not discuss further).

Lines 269-283 show discovered links found between the entities in the input text.
 
In the LispWorks CAPI user interface developed in the next chapter I use two text output stream window panes, one for the generated SPARQL and one for the results.

## Text User Interface Implementation

We will skip looking at the **kgn-text-ui.asd** and **package.lisp** files for this library but look at **src/kgn-text-ui/kgn-text-ui.lisp** in its entirety. When entities are identified in input text we find candidate DBPedia entity URIs that we present to the user. We precede each entire DBPedia description with an index starting at 0. The user enters the indices for entities to further process. For example, in the example listing in the previous section I entered "8 9" to indicate two company URIs.


{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:kgn-text-ui)

(defun pprint-results (results)
  (dolist (result (car results))
    (terpri)
    (format t  "~A:" (first result))
    (format t " ~A~%" (second result))))


(defun multiple-selections (sel-list)
  (if (not (null sel-list))
      (let ()
        (pprint sel-list)
        (format t
         "~%- - - - Enter zero or more indices for your desired selections:~%~%")
        (let ((count 0))
          (dolist (sel sel-list)
            (format t "~A  -   ~S ~%~%" count (cadr (assoc :comment (car sel))))
            (setf count (1+ count))))
        (let* ((line (read-line))
               (indices
                (if (> (length line) 0)
                    (mapcar
                     #'parse-integer
                     (myutils:tokenize-string line)))))
          (print indices)
    ;(dolist (index indices)
    ;  (setf ret (cons (nth index str-list)
          indices))))

;; (kgn-text-ui::multiple-selections
;;   '("Option 1" "Option 2" "And yet another option 3"))


(defun prompt-selection-list (a-list-of-choices) 
  ;; e.g., '((:people (("11" "data1")  ("22" "data2"))) (:places (("p1" "data3"))))
  (let (ret)
    (dolist (choice a-list-of-choices)
      (setf choice (remove-if #'null choice))
      (let* ((topic-type (car choice))
             (choice-list-full (rest choice))
             (choice-list (remove-duplicates
                           (map 'list #'(lambda (z)
                                          (list
                                           z
                                           (string-shorten
                                            (kgn-common:clean-comment
                                             (kgn-common:clean-comment (cadr z)))
                                            140 :first-remove-stop-words t)))
                                ;; top level list flatten:
                                (apply #'append choice-list-full))
                           :test #'equal)))
        (let (ret2
              (dialog-results (multiple-selections choice-list)))
          (dolist (index dialog-results)
            (setf ret2 (cons (nth index choice-list) ret2)))
          (if (> (length ret2) 0)
              (setf ret (cons (list topic-type (reverse ret2)) ret))))))
    (reverse ret)))

;; (kgn-text-ui::prompt-selection-list 
;;   '((:people (("11" "data1")  ("22" "data2")))
;;     (:places (("p1" "data3") ("p2" "data4") ("p3" "data5")))))
;; (kgn-text-ui::prompt-selection-list
;;   (get-entity-data-helper "Bill Gates went to Seattle to Microsoft"))

(defun colorize-sparql (str &key (stream t))
  " this could be used to colorize text (as it is in kgn-capi-ui example)"
  ;;(declare (ignore message-stream))
  (declare (ignore stream))
  (format t "~%~S~%" str))

(defun get-string-from-user (text-prompt)
  (format t "~%~S:~%" text-prompt)
  (read-line))


;; Main funtion

(defun kgn-text-ui ()
  (let (prompt
        (message-stream t)
        (results-stream t))
    (loop
       while
        (>
         (length
           (setf prompt
             (get-string-from-user
               "Enter entity names (people, places, companies, etc.")))
           0)
         do
         (let* ((entity-data (get-entity-data-helper prompt :message-stream t)))
           (let ((user-selections (prompt-selection-list entity-data)))
             (dolist (ev user-selections)
               (if (> (length (cadr ev)) 0)
                   (let ()
                     (terpri results-stream)
                     (format results-stream "- - - ENTITY TYPE: ~A - - -" (car ev))
                     ;;(terpri results-stream)
                     (dolist (uri (cadr ev))
                       (setf uri (assoc :s (car uri)))
                       (case (car ev)
                         (:people
                          (pprint-results 
                           (kgn-common:dbpedia-get-person-detail  
                             uri
                             :message-stream message-stream
                             :colorize-sparql-function #'colorize-sparql)))
                         (:companies
                          (pprint-results 
                           (kgn-common:dbpedia-get-company-detail uri
                             :message-stream message-stream
                             :colorize-sparql-function #'colorize-sparql)))
                         (:countries
                          (pprint-results
                           (kgn-common:dbpedia-get-country-detail uri
                             :message-stream message-stream
                             :colorize-sparql-function #'colorize-sparql)))
                         (:cities
                          (pprint-results
                           (kgn-common:dbpedia-get-city-detail    uri
                             :message-stream message-stream
                             :colorize-sparql-function #'colorize-sparql)))
                         (:products
                          (pprint-results
                           (kgn-common:dbpedia-get-product-detail uri
                           :message-stream message-stream
                           :colorize-sparql-function #'colorize-sparql)))))))))))))

;; (kgn-text-ui:kgn-text-ui)
~~~~~~~~

The utility function **multiple-selections** listed in lines 10-29 displays a list of user choices, adding a zero-based index for each list item. The user can enter zero or more indices to indicate their choices using the function **prompt-selection-list** listed in lines 35-59.

The commented out code in lines 61-65 can be used to test these two functions.

The main function **kgn-text-ui** is listed in lines 80-129.

## Wrap-up

In the previous chapter we implemented the Knowledge Graph Navigator library. Here we developed a text-based user interface. In the next chapter we use the library to develop a LispWorks specific CAPI user interface.
