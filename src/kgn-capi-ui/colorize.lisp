(in-package #:kgn-capi-ui)

(defun colorize-sparql (s  &key (stream nil))
  (let ((tokens (myutils:tokenize-string-keep-uri (myutils:replace-all s "@@" " @@ ")))
        in-var)
    (dolist (token tokens)
      (if (> (length token) 0)
          (if (or in-var (equal token "?"))
              (capi::write-string-with-properties token '(:highlight :compiler-warning-highlight) stream)
            (if (find token '("where" "select" "distinct" "option" "filter" "FILTER" "OPTION" "DISTINCT" "SELECT" "WHERE") :test #'equal)
                (capi::write-string-with-properties token '(:highlight :compiler-note-highlight) stream)
              (if (equal (subseq token 0 1) "<")
                  (capi::write-string-with-properties token '(:highlight :bold) stream)
                (if (equal token "@@")
                    (terpri stream)
                  (if (not (equal token "~")) (write-string token stream)))))))
      (if (equal token "?")
          (setf in-var t)
        (setf in-var nil))
      (if (and
           (not in-var)
           (not (equal token "?")))
          (write-string " " stream)))
    (terpri stream)))

;;(colorize-sparql "select ?s ?p  where {@@  ?s ?p \"Microsoft\" } @@  FILTER (lang(?comment) = 'en')")
;;(pprint (myutils:tokenize-string-keep-uri "select ?s ?p ?o where {@@  ?s ?p ?o } @@  FILTER (lang(?comment) = 'en')"))
#|
(pprint (myutils:tokenize-string "select ?s ~% where ~% foo.bar"))

(colorize-sparql "select ?s ?p ?o where {~%  ?s ?p ?o }~%")

(colorize-sparql "select distinct ?s ?comment { ?s ?p \"Bill\"@en . @@ ~
                        ?s <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment  . FILTER  (lang(?comment) = 'en') . @@ ~
                        ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> Bill . @@ ~
                      } LIMIT 15")
|#
