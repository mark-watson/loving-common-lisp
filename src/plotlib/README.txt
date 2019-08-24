This is the stub README.txt for the "webscrape" project.

try:

        (ql:quickload "plotlib")
        (plotlib::test-plotlib "test-plotlib.png")
        
or:

    (ql:quickload "plotlib")
    (plotlib:plot-string "test 1 2 3")
    (plotlib:with-canvas (:width 90 :height 90)
      (plotlib:plot-string 10 65 "test 1 2 3")
      (plotlib:save-png "test1.png")))

TBD: fix last example!!
