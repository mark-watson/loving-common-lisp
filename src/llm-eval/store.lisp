;;;; store.lisp -- keep eval runs in SQLite so we can compare runs over time.

(in-package #:llm-eval)

(defun open-store (path)
  "Open (and create if needed) the SQLite database at PATH. Use
\":memory:\" for a throwaway database."
  (let ((db (sqlite:connect path)))
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS runs (
         id INTEGER PRIMARY KEY,
         created TEXT NOT NULL,
         label TEXT NOT NULL,
         config TEXT)")
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS results (
         run_id INTEGER NOT NULL REFERENCES runs(id),
         case_id TEXT NOT NULL,
         answer TEXT,
         exact REAL, f1 REAL, keywords REAL, judge REAL,
         judge_reason TEXT,
         seconds REAL,
         error TEXT)")
    db))

(defun timestamp ()
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun save-run (db label config results)
  "Store RESULTS under a new run and return the run id. LABEL names the run
and CONFIG describes the settings, for example the model name."
  (sqlite:with-transaction db
    (sqlite:execute-non-query db
      "INSERT INTO runs (created, label, config) VALUES (?, ?, ?)"
      (timestamp) label (prin1-to-string config))
    (let ((run-id (sqlite:last-insert-rowid db)))
      (dolist (result results)
        (sqlite:execute-non-query db
          "INSERT INTO results (run_id, case_id, answer, exact, f1, keywords,
                                judge, judge_reason, seconds, error)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          run-id
          (eval-result-case-id result)
          (eval-result-answer result)
          (eval-result-exact result)
          (eval-result-f1 result)
          (eval-result-keywords result)
          (eval-result-judge result)
          (eval-result-judge-reason result)
          (eval-result-seconds result)
          (eval-result-error result)))
      run-id)))

(defun run-summaries (db)
  "One row per run: id, label, created, case count, and mean scores."
  (sqlite:execute-to-list db
    "SELECT r.id, r.label, r.created, COUNT(*),
            AVG(x.f1), AVG(x.keywords), AVG(x.judge)
     FROM runs r JOIN results x ON x.run_id = r.id
     GROUP BY r.id ORDER BY r.id"))

(defun print-run-summaries (db &optional (stream *standard-output*))
  (format stream "~&~3A ~22A ~19A ~5A ~5A ~5A ~5A~%"
          "id" "label" "created" "cases" "f1" "keys" "judge")
  (dolist (row (run-summaries db))
    (destructuring-bind (id label created cases f1 keywords judge) row
      (format stream "~3D ~22A ~19A ~5D ~5,2F ~5,2F ~5,2F~%"
              id label created cases f1 keywords (or judge 0)))))

(defun compare-runs (db old-run new-run &key (metric "judge") (threshold 0))
  "Compare METRIC, a column name of the results table, between two runs.
Returns two lists of (case-id old-score new-score): regressions, where the
score dropped by more than THRESHOLD, and improvements, where it rose by
more than THRESHOLD."
  (unless (member metric '("exact" "f1" "keywords" "judge") :test #'string=)
    ;; The column name goes into the SQL text, so only allow known names.
    (error "Unknown metric ~S" metric))
  (let ((rows (sqlite:execute-to-list db
                (format nil "SELECT a.case_id, a.~A, b.~A
                             FROM results a JOIN results b ON a.case_id = b.case_id
                             WHERE a.run_id = ? AND b.run_id = ?
                             ORDER BY a.case_id" metric metric)
                old-run new-run)))
    (flet ((change (row) (- (or (third row) 0) (or (second row) 0))))
      (values (remove-if-not (lambda (row) (< (change row) (- threshold))) rows)
              (remove-if-not (lambda (row) (> (change row) threshold)) rows)))))

(defun print-comparison (db old-run new-run &key (metric "judge") (threshold 0)
                                                 (stream *standard-output*))
  (multiple-value-bind (regressions improvements)
      (compare-runs db old-run new-run :metric metric :threshold threshold)
    (format stream "~&Run ~D -> run ~D, metric ~A~%" old-run new-run metric)
    (format stream "Regressions:~:[ none~;~:*~{~%  ~{~A: ~,2F -> ~,2F~}~}~]~%" regressions)
    (format stream "Improvements:~:[ none~;~:*~{~%  ~{~A: ~,2F -> ~,2F~}~}~]~%" improvements)
    (values regressions improvements)))
