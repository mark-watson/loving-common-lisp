;;
 ;  Hopfield neural network example from the chapter
 ;  "Hopfield Neural Networks". The functions Hopfield-Init and
 ;  HopfieldNetRecall are the neural network code from the chapter
 ;  listings; the test driver at the end is a non-graphics version
 ;  of the chapter's test function.
 ;;

;;
 ;  The arrays and counters describing a Hopfield network are held in
 ;  these special variables so that the inner (lexically scoped)
 ;  functions defined by Hopfield-Init and HopfieldNetRecall can see
 ;  them.
 ;;
(defvar *num-inputs*)
(defvar *num-training-examples*)
(defvar *training-list*)
(defvar *inputCells*)
(defvar *tempStorage*)
(defvar *HopfieldWeights*)

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

(defun HopfieldNetRecall (aHopfieldNetwork numberOfIterations)
  (let ((*num-inputs* (nth 0 aHopfieldNetwork))
        (*num-training-examples*  (nth 1 aHopfieldNetwork))
        (*training-list* (nth 2 aHopfieldNetwork))
        (*inputCells* (nth 3 aHopfieldNetwork))
        (*tempStorage* (nth 4 aHopfieldNetwork))
        (*HopfieldWeights* (nth 5 aHopfieldNetwork)))

    (defun deltaEnergy (row-index y &aux (temp 0.0))  ;; lexically scoped
      (dotimes (j *num-inputs*)
        (setq temp (+ temp (* (aref *HopfieldWeights* row-index j) (aref y j)))))
      (- (* 2.0 temp) (aref *tempStorage* row-index)))

    (dotimes (ii numberOfIterations) ;; main code
      (dotimes (i *num-inputs*)
        (setf (aref *inputCells* i)
              (if (> (deltaEnergy i *inputCells*) 0)
                  1
                  0))))))

;;
 ;  Non-graphics version of the chapter's test function. It uses the
 ;  same three training patterns, scrambles a copy of each one, lets
 ;  the network settle, and prints the original, scrambled, and
 ;  reconstructed patterns.
 ;;
(defun test (&aux aHopfieldNetwork)
  (let ((tdata '(  ;; sample sine wave data with different periods:
                 (1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 0 0)
                 (0 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 1 0)
                 (0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 1 1 0 1 1))))
    (print tdata)
    (setq aHopfieldNetwork (Hopfield-Init tdata))

    ;; lexically scoped variables are accessible by inner functions:
    (let ((*num-inputs* (nth 0 aHopfieldNetwork))
          (*num-training-examples*  (nth 1 aHopfieldNetwork))
          (*training-list* (nth 2 aHopfieldNetwork))
          (*inputCells* (nth 3 aHopfieldNetwork))
          (*tempStorage* (nth 4 aHopfieldNetwork))
          (*HopfieldWeights* (nth 5 aHopfieldNetwork)))

      (defun modifyInput (arrSize arr)  ;; modify input array for testing
        (dotimes (i arrSize)
          (if (< (random 50) 5)
              (if (> (aref arr i) 0)
                  (setf (aref arr i) -1)
                  (setf (aref arr i) 1)))))

      (defun printPattern (arr label)
        (format t "~a: " label)
        (dotimes (i (array-dimension arr 0))
          (princ (if (> (aref arr i) 0) 1 0)))
        (terpri))

      ;; Test network on training data that is randomly modified:

      (dotimes (iter 10) ;; cycle 10 times
        (dotimes (s *num-training-examples*)
          (dotimes (i *num-inputs*)
            (setf (aref *inputCells* i) (aref *training-list* s i)))
          (format t "~&Iteration ~a exemplar ~a~%" iter s)
          (printPattern *inputCells* "Original ")
          (modifyInput *num-inputs* *inputCells*)
          (printPattern *inputCells* "Scrambled")
          (dotimes (call-net 5) ;; iterate Hopfield net 5 times
            (HopfieldNetRecall aHopfieldNetwork 1))  ;; calling with 1 iteration
          (printPattern *inputCells* "Recalled "))))))
