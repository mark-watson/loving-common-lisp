;; Common LISP Hopfield Network for associative memory recall.
;; Copyright 1989, 2017 by Mark Watson

;; Note: for more detailed comments and explanations of the code, please see
;; the book [Loving Common Lisp, or the Savvy Programmer's Secret Weapon](https://leanpub.com/lovinglisp)
;; that can be read free online and is released under a Creative Commons License

(ql:quickload "plotlib")

(defpackage #:hopfield
  (:use #:cl #:plotlib))  ;; TBD: define exports

(in-package #:hopfield)

(proclaim '(special *num-inputs* *num-training-examples* *training-list*
            *inputCells* *tempStorage* *HopfieldWeights*))
            
(defun Hopfield-Init (training-data
                      &aux temp *num-inputs* *num-training-examples*
                        *training-list* *inputCells* *tempStorage*
                        *HopfieldWeights*)

  (setq *num-inputs* (length (car training-data)))
  (setq *num-training-examples* (length training-data))

  (setq *training-list* (make-array (list *num-training-examples* *num-inputs*)))
  (setq *inputCells* (make-array (list *num-inputs*)))
  (setq *tempStorage* (make-array (list *num-inputs*)))
  (setq *HopfieldWeights* (make-array (list *num-inputs* *num-inputs*)))

  (dotimes (j *num-training-examples*) ;; copy training data
    (dotimes (i *num-inputs*)
      (setf
       (aref *training-list* j i)
       (nth i (nth j training-data)))))

  (defun adjustInput (value)  ;; this function is lexically scoped
    (if (< value 0.1)
        -1.0
        +1.0))

  (dotimes (i *num-inputs*) ;; adjust training data
    (dotimes (n *num-training-examples*)
      (setf
       (aref *training-list* n i)
       (adjustInput (aref *training-list* n i)))))

  (dotimes (i *num-inputs*) ;; zero weights
    (dotimes (j *num-inputs*)
      (setf (aref *HopfieldWeights* i j) 0)))

  (dotimes (j-1 (- *num-inputs* 1)) ;; autocorrelation weight matrix
    (let ((j (+ j-1 1)))
      (dotimes (i j)
        (dotimes (s *num-training-examples*)
          (setq temp
                (truncate
                 (+
                  (*  ;; 2 if's truncate values to -1 or 1:
                   (adjustInput (aref *training-list* s i))
                   (adjustInput (aref *training-list* s j)))
                  (aref *HopfieldWeights* i j))))
          (setf (aref *HopfieldWeights* i j) temp)
          (setf (aref *HopfieldWeights* j i) temp)))))
  (dotimes (i *num-inputs*)
    (setf (aref *tempStorage* i) 0)
    (dotimes (j i)
      (setf (aref *tempStorage* i)
            (+ (aref *tempStorage* i) (aref *HopfieldWeights* i j)))))

  (list ;; return the value of the Hopfield network data object
   *num-inputs* *num-training-examples* *training-list*
   *inputCells* *tempStorage* *HopfieldWeights*))


;;
                                        ; Iterate the network to let it settle in a stable pattern
                                        ; which will (we hope) be the original training pattern
                                        ; most closely resembling the noisy test pattern:
;;

(defun HopfieldNetRecall (aHopfieldNetwork numberOfIterations)
  (let ((*num-inputs* (nth 0 aHopfieldNetwork))
        (*num-training-examples*  (nth 1 aHopfieldNetwork))
        (*training-list* (nth 2 aHopfieldNetwork))
        (*inputCells* (nth 3 aHopfieldNetwork))
        (*tempStorage* (nth 4 aHopfieldNetwork))
        (*HopfieldWeights* (nth 5 aHopfieldNetwork)))

    (defun deltaEnergy (row-index y &aux (temp 0.0))  ;; this function is lexically scoped
      (dotimes (j *num-inputs*)
        (setq temp (+ temp (* (aref *HopfieldWeights* row-index j) (aref y j)))))
      (- (* 2.0 temp) (aref *tempStorage* row-index)))

    (dotimes (ii numberOfIterations) ;; main code
      (dotimes (i *num-inputs*)
        (setf (aref *inputCells* i)
              (if (> (deltaEnergy i *inputCells*) 0)
                  1
                  0))))))

(defvar plot-size 8)
(defvar plot-size+1 (+ plot-size 1))

(defun test (&aux aHopfieldNetwork)
  (let ((tdata '(  ;; sample sine wave data with different periods:
                 (1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0)
                 (0 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 1 0)
                 (0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 1 1 0 1 1)))
        (width 300)
        (height 180))
    (vecto::with-canvas (:width width :height height)
      (plotlib:plot-string-bold 10 (- height 14) "Hopfield pattern classifier")

      ;; Set up network:
      (print tdata)
      (setq aHopfieldNetwork (Hopfield-Init tdata))

      ;; lexically scoped variables are accesible by inner functions:
      (let ((*num-inputs* (nth 0 aHopfieldNetwork))
            (*num-training-examples*  (nth 1 aHopfieldNetwork))
            (*training-list* (nth 2 aHopfieldNetwork))
            (*inputCells* (nth 3 aHopfieldNetwork))
            (*tempStorage* (nth 4 aHopfieldNetwork))
            (*HopfieldWeights* (nth 5 aHopfieldNetwork)))

        (defun plotExemplar (row &aux (dmin 0.0) (dmax 1.0) (x 20) (y 40))
          (let ((YSize (array-dimension *training-list* 1)))
            (plotlib:plot-string (+ x 20) (- height (- y 10)) "Original Training Exemplar")
            (dotimes (j Ysize)
              (plotlib:plot-fill-rect
               (+ x (* j plot-size+1)) (- height y) plot-size plot-size
               (truncate (* (/  (- (aref *training-list* row j) dmin) (- dmax dmin)) 5)))
              (plotlib:plot-frame-rect (+ x (* j plot-size+1)) (- height y) plot-size plot-size))))

        (defun plot-original-inputCells (&aux (dmin 0.0) (dmax 1.0) (x 20) (y 80))
          (let ((Xsize (array-dimension *inputCells* 0)))
            (plotlib:plot-string (+ x 20) (- height (- y 10)) "Scrambled Inputs")
            (dotimes (j Xsize)
              (plotlib:plot-fill-rect
               (+ x (* j plot-size+1)) (- height y) plot-size plot-size
               (truncate (* (/  (- (aref *inputCells* j) dmin) (- dmax dmin)) 5)))
              (plotlib:plot-frame-rect (+ x (* j plot-size+1)) (- height y) plot-size plot-size))))

        (defun plot-inputCells (&aux (dmin 0.0) (dmax 1.0) (x 20) (y 120))
          (let ((Xsize (array-dimension *inputCells* 0)))
            (plotlib:plot-string (+ x 20) (- height (- y 10)) "Reconstructed Inputs")
            (dotimes (j Xsize)
              (plotlib:plot-fill-rect
               (+ x (* j plot-size+1)) (- height y) plot-size plot-size
               (truncate (* (/  (- (aref *inputCells* j) dmin) (- dmax dmin)) 5)))
              (plotlib:plot-frame-rect (+ x (* j plot-size+1)) (- height y) plot-size plot-size))))

        (defun modifyInput (arrSize arr)  ;; modify input array for testing
          (dotimes (i arrSize)
            (if (< (random 50) 5)
                (if (> (aref arr i) 0)
                    (setf (aref arr i) -1)
                    (setf (aref arr i) 1)))))

        ;; Test network on training data that is randomly modified:

        (dotimes (iter 10) ;; cycle 10 times and make 10 plots
          (dotimes (s *num-training-examples*)
            (dotimes (i *num-inputs*)
              (setf (aref *inputCells* i) (aref *training-list* s i)))
            (plotExemplar s)
            (modifyInput *num-inputs* *inputCells*)
            (plot-original-inputCells)
            (dotimes (call-net 5) ;; iterate Hopfield net 5 times
              (HopfieldNetRecall aHopfieldNetwork 1)  ;; calling with 1 iteration
              (plot-inputCells)))

          (vecto::save-png
           (concatenate 'string
                        "output_plot_hopfield_nn_" (format nil "~5,'0d" iter) ".png")))))))

(princ "There is 1 test function for the Hopfield neural network model. Try:")
(terpri)
(princ "(in-package #:hopfield)")
(terpri)
(princ "(test)")
(terpri)
