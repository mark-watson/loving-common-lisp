# install

(ql:quickload :cl-dbi)

# bash command line:

sqlite3 test.db "create table test (id integer primary key, str text);"
sqlite3 test.db "INSERT INTO test VALUES (1, 'cat stand');"
sqlite3 test.db "SELECT * FROM test;"
