;;;; rag-golden.lisp -- golden dataset for the RAG example in src/RAG.
;;;; Each answer comes from the files in src/RAG/data. KEYWORDS lists the
;;;; facts a correct answer must contain.

(:id "co2-preindustrial"
 :question "What was the pre-industrial concentration of atmospheric CO2?"
 :reference "About 280 parts per million (ppm)."
 :keywords ("280"))

(:id "co2-2024"
 :question "What level had atmospheric CO2 reached as of 2024?"
 :reference "Over 420 ppm, a 50% increase over pre-industrial levels."
 :keywords ("420"))

(:id "warming-so-far"
 :question "How much have global average temperatures risen since the pre-industrial era?"
 :reference "About 1.1 degrees C (2.0 degrees F)."
 :keywords ("1.1"))

(:id "greenland"
 :question "How much would global sea levels rise if the Greenland ice sheet melted completely?"
 :reference "About 7.2 meters (24 feet)."
 :keywords ("7.2"))

(:id "dac-cost"
 :question "What does direct air capture currently cost per ton of CO2 removed?"
 :reference "Between $250 and $600 per ton of CO2."
 :keywords ("250" "600"))

(:id "leaf-range"
 :question "How much range did the 2011 Nissan Leaf offer?"
 :reference "About 73 miles."
 :keywords ("73"))

(:id "battery-manufacturing-co2"
 :question "How much CO2 does EV battery manufacturing produce per kilowatt-hour of battery capacity?"
 :reference "Approximately 75 to 100 kg of CO2 per kilowatt-hour."
 :keywords ("75" "100"))

(:id "solid-state-companies"
 :question "Which companies are leading the development of solid-state batteries?"
 :reference "Toyota, Samsung SDI, and QuantumScape."
 :keywords ("toyota" "samsung" "quantumscape"))

(:id "storage-cost-drop"
 :question "How much has the cost of lithium-ion battery storage fallen since 2010?"
 :reference "About 90%, from over $1,100 per kilowatt-hour to under $140 per kilowatt-hour."
 :keywords ("90" "140"))

(:id "denmark-wind"
 :question "What share of Denmark's electricity comes from wind power?"
 :reference "Over 50%."
 :keywords ("50"))

(:id "hydro-share"
 :question "What share of world electricity production comes from hydroelectric power?"
 :reference "About 16%."
 :keywords ("16"))

(:id "storage-roles"
 :question "What is the most common form of grid-scale energy storage, and what role does it play next to batteries in a hybrid storage system?"
 :reference "Pumped-storage hydroelectricity. It provides bulk, long-duration storage over hours or days, while batteries handle short, fast, frequent charge cycles."
 :keywords ("pumped"))
