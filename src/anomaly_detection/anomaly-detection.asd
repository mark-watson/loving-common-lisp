(defsystem "anomaly-detection"
  :version "0.1.0"
  :author "Mark Watson"
  :license "Apache 2.0"
  :components ((:file "anomaly-detection")
               (:file "wisconsin" :depends-on ("anomaly-detection")))
  :description
  "Gaussian anomaly detection with epsilon tuning.")
