(ql:quickload "sqlite")

(defparameter *db*
  (sqlite:connect "example.db"))

(sqlite:execute-non-query
 *db*
 "CREATE TABLE IF NOT EXISTS documents (id INTEGER PRIMARY KEY, text TEXT, chunks TEXT, chunk_embeddings TEXT, meta_data TEXT)")

(sqlite:execute-non-query
 *db*
 "INSERT INTO documents (text, chunks, chunk_embeddings, meta_data) VALUES (?, ?, ?, ?)"
 "example text" "example chunks" "example chunk embeddings" "example meta data")

(sqlite:execute-to-list *db* "select * from documents")

(sqlite:disconnect *db*)
