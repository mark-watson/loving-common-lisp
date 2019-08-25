;; File DeltaRule.Lisp
;; Copyright 1990, 2017 by Mark Watson

;; Note: for more detailed comments and explanations of the code, please see
;; the book [Loving Common Lisp, or the Savvy Programmer's Secret Weapon](https://leanpub.com/lovinglisp)
;; that can be read free online and is released under a Creative Commons License

;;;;;;;;;;;;; Externally (callable) functions in this file: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (NewDeltaNetwork sizeList)
;       Args:   sizeList = list of sizes of slabs. This also defines
;                          the number of slabs in the network.
;                          (e.g.,  '(10 5 4) ==> a 3-slab network with 10
;                           input neurons, 5 hidden neurons, and 4 output
;                           neurons).
;
;       Returned value = a list describing the network:
;          (nLayers sizeList
;           (activation-array[1] .. activation-array[nLayers])
;           (weight-array[2] .. weight-array[nLayers])
;           (sum-of-products[2] .. sum-of-products[nLayers[nLayers])
;           (back-prop-error[2] .. back-prop-error[nLayers]))
;           (old-delta-weights[2] .. for momentum term

; (DeltaLearn networkList trainingList)
;       Args:    output-plot-file-name a string to plot, nil to not plot
;                networkList  = list returned from function  NewDeltaNetwork
;                trainingList = a list of lists of training exemplars.
;                               For example, a list might be:
;                               (((0 1) (1 0))  ; first exemplar
;                                ((1 0) (0 1))) ; second exemplar
;                               Note: the inner sub-lists can also be arrays.
;                nIterations  = number of complete training iterations
;
;       Returned value = average error at output neurons for last
;                        training cycle.

; (DeltaRecall networkList inputList)
;       Args:    networkList  = list returned from function 'NewDeltaNetwork'
;                inputList    = list OR array of input activation values
;
;       Returned value = list of output neuron values

; (DeltaPlot networkList) ==> plots a network. Must call '(init-plot) first.

; (WriteDeltaNetwork fileName networkList) ==> saves a network to disk


; (ReadDeltaNetwork fileName) ==> returns a network list

;;;;;;;;;;;;;;;;;;; End of list of externally callable functions ;;;;;;;;;;;;;

(ql:quickload "plotlib")

(defpackage #:deltarule
  (:use #:cl #:plotlib))  ;; TBD: define exports

(proclaim '(special eidaList defaultEidaList *delta-default-input-noise-value*
            *delta-rule-debug-flag* ))

;; Define default learning rates for each layer of neurons:

(setq defaultEidaList '(0.5 0.4 0.3 0.2 0.08 0.07))

;; Define the default noise to add to each input neuron:

(setq *delta-default-input-noise-value* 0.08)
(setq *delta-rule-debug-flag* nil)


;;
 ; Utilities to plot a network
 ;;

(defun plotActivations (title x y data dmin dmax height)
  (let ((size (array-dimension data 0)) (ypos 0) (xpos x))
    (plotlib:plot-string x (- height (- y 10)) title)
    (dotimes (i size)
      (if (< size 20)
    (setq ypos y xpos (+ x (* i 9)))
    (if (< i (/ size 2))
        (setq ypos (- y 7) xpos (+ x (* i 9)))
        (setq ypos (+ y 2)  xpos (+ x (* (- i (/ size 2)) 9)))))
      (plotlib:plot-size-rect
       (truncate xpos) (- height (truncate ypos)) 8 8
       (min
  8
  (max
   0
   (truncate (* (/  (- (aref data i) dmin) (- dmax dmin)) 8)))))
      (plotlib:plot-frame-rect (truncate xpos) (- height (truncate ypos)) 8 8))))

(defun plotWeights (title x y data dmin dmax deltaWeights height)
  (let ((Xsize (array-dimension data 0))
        (YSize (array-dimension data 1)))
    (if (< (* Xsize Ysize) 3000)     ;; don't try to plot very large weight sets
  (progn
    (plotlib:plot-string (+ x 20) (- height (- y 10)) title)
    (dotimes (i Xsize)
      (dotimes (j Ysize)
        (plotlib:plot-size-rect
         (+ x (* i 9)) (- height (+ y (* j 9))) 8 8
         (min
    8
    (max
     0
     (truncate (* (/  (- (aref data i j) dmin) (- dmax dmin)) 8)))))
        (plotlib:plot-frame-rect (+ x (* i 9)) (- height (+ y (* j 9))) 8 8)
        (plotlib:plot-size-rect
         (+ (* Xsize 8) 30 x (* i 9)) (- height (+ y (* j 9))) 8 8
         (min
    8
    (max
     0
     (truncate (* (/  (- (aref deltaWeights i j) -.05) (- .05 -.05)) 8)))))
        (plotlib:plot-frame-rect (+ (* Xsize 8) 30 x (* i 9)) (- height (+ y (* j 9))) 8 8)))))))

(defun DeltaPlot (netList plot-output-file-name)
  (let ((nLayers (car netList))
  (activationList (caddr netList))
  (weightList (cadr (cdddr netList)))
  (deltaWeightList (caddr (cdddr netList)))
  (minScale 0.0) ;; -0.3)
  (maxScale 0.6) ;; 0.3)
  (n 0)
  (y-start 0)
  (width 600)
  (height 600))
    (vecto::with-canvas (:width width :height height)
      (plotlib:plot-string-bold 20 (- height 15) "Delta Network")
      (plotlib:plot-string 20 (- height 30) "Activation slabs:")
      (plotActivations "slab1"
           10
           60
           (nth 0 ActivationList)
           0.0 ;; lower limit of Sigmoid function
           1.0 ;; upper limit of Sigmoid function
           height)
      (setq y-start 60)
      (dotimes (n-1 (- nLayers 1))
  (setq n (+ n-1 1))
  (if (equal n (- nLayers 1))
      (setq minScale -0.1 maxScale 0.1)) ; scale up output display
  (plotActivations
   (nth n '("slab1" "slab2" "slab3" "slab4" "slab5" "slab6")) ; title
   10 ; x location for subplot
   (+ y-start (* n 35)) ; y location for subplot
   (nth n ActivationList) ; data to plot as gray scale
   minScale
   maxScale
   height))
      (if (< nLayers 6)
    (progn
      (setq y-start (+ y-start (* nLayers 35)))
      (plotlib:plot-string 20 (- height y-start) "Weights and Delta Weights:")
      (setq y-start (+ y-start 22))
      (dotimes (n (- nLayers 1))
        (plotWeights
         (nth n '("slab1 -> slab2"  "slab2 -> slab3" "slab3 -> slab4" "slab4 -> slab5" "slab5 -> slab6"))
         10 (+ y-start (* n 70)) ; x,y position of subplot
         (nth n WeightList)
         -1.0 1.0
         (nth n deltaWeightList)
         height))))
      (vecto::save-png plot-output-file-name))))

;;
 ; Calculate Sigmoid and derivative of Sigmoid functions:
 ;;

(defun Sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun dSigmoid (x)
  (let ((temp (Sigmoid x)))
    (* temp (- 1.0 temp))))


;;
 ; Generate floating point random numbers:
 ;;

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

;;
 ; Create a new delta network:
 ;;

;; alpha = coefficient for new weight change
;; beta  = coefficient for adding in last weight change

(defun NewDeltaNetwork (sizeList &optional (alpha 0.2) (beta 0.8))
  (let ((numLayers (length sizeList))
        (w-list nil)        ; weights
        (dw-list nil)       ; delta weights
        (old-dw-list nil)   ; old delta weights for momentum terms
        (a-list nil)        ; activation values
        (s-list nil)        ; sum of products
        (d-list nil))       ; back propagated deltas

    (setq eidaList defaultEidaList)

     ;;
      ; Initialize storage for activation energy for all slabs:
      ;;
    (setq a-list
          (mapcar
           (lambda (size) (make-array (list size) :element-type 'float :initial-element 0.0))
           sizeList))

    ;;
     ; Initialize storage for sum of products arrays:
     ;;
    (setq s-list
          (mapcar
           (lambda (size) (make-array (list size) :element-type 'float :initial-element 0.0))
           (cdr sizeList)))

    ;;
     ; Initialize storage for delta arrays:
     ;;
    (setq d-list
          (mapcar
           (lambda (size) (make-array (list size) :element-type 'float :initial-element 0.0))
           (cdr sizeList)))

    ;;
     ; Initialize storage for the weights:
     ;;
    (dotimes (i (- numLayers 1))
      (setq
       w-list
       (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) w-list)))
    (setq w-list
          (mapcar
           (lambda (size) (make-array size :element-type 'float :initial-element 0.0))
           (reverse w-list)))

    ;;
     ; Initialize the storage for delta weights:
     ;;
    (dotimes (i (- numLayers 1))
      (setq
       dw-list
       (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) dw-list)))
    (setq dw-list
          (mapcar
           (lambda (size) (make-array size :element-type 'float))
           (reverse dw-list)))

    ;;
     ; Initialize the storage for old delta weights:
     ;;
    (dotimes (i (- numLayers 1))
      (setq
       old-dw-list
       (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) old-dw-list)))
    (setq old-dw-list
          (mapcar
           (lambda (size) (make-array size  :element-type 'float
                                       :initial-element 0.0))
           (reverse old-dw-list)))

    ;;
     ;  Initialize values for all activations:
     ;;
    (mapc
     (lambda (x)
        (let ((num (array-dimension x 0)))
          (dotimes (n num)
            (setf (aref x n) (frandom 0.01 0.1)))))
     a-list)

    ;;
     ;  Initialize values for all weights:
     ;;
    (mapc
     (lambda (x)
        (let ((numI (array-dimension x 0))
              (numJ (array-dimension x 1)))
          (dotimes (j numJ)
            (dotimes (i numI)
              (setf (aref x i j) (frandom -0.5 0.5))))))
     w-list)
    (list numLayers sizeList a-list s-list w-list dw-list
          d-list old-dw-list alpha beta)))

;;
 ;  Utility function for training a delta rule neural network.
 ;  The first argument is the name of an output PNG plot file
 ;  and a nil value turns off plotting the network during training.
 ;  The second argument is a network definition (as returned from
 ;  NewDeltaNetwork), the third argument is a list of training
 ;  data cases (see the example test functions at the end of this
 ;  file for examples.
 ;;

(defun DeltaLearn (plot-output-file-name
       netList trainList)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (sumOfProductsList (car (cdddr netList)))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (deltaList (cadddr (cdddr netList)))
        (oldDeltaWeightList (cadddr (cdddr (cdr netList))))
        (alpha (cadddr (cdddr (cddr netList))))
        (beta (cadddr (cdddr (cdddr netList))))
        (inputs nil)
        (targetOutputs nil)
        (iDimension nil)
        (jDimension nil)
        (iActivationVector nil)
        (jActivationVector nil)
        (n nil)
        (weightArray nil)
        (sumOfProductsArray nil)
        (iDeltaVector nil)
        (jDeltaVector nil)
        (deltaWeightArray nil)
        (oldDeltaWeightArray nil)
        (sum nil)
        (iSumOfProductsArray nil)
        (error nil)
        (outputError 0)
        (delta nil)
        (eida nil)
        (inputNoise 0))

    ;;
     ; Zero out deltas:
     ;;
    (dotimes (n (- nLayers 1))
      (let* ((dw (nth n deltaList))
             (len1 (array-dimension dw 0)))
        (dotimes (i len1)
          (setf (aref dw i) 0.0))))

    ;;
     ; Zero out delta weights:
     ;;
    (dotimes (n (- nLayers 1))
      (let* ((dw (nth n deltaWeightList))
             (len1 (array-dimension dw 0))
             (len2 (array-dimension dw 1)))
        (dotimes (i len1)
          (dotimes (j len2)
            (setf (aref dw i j) 0.0)))))

    (setq inputNoise *delta-default-input-noise-value*)

    ;;
     ;  Main loop on training examples:
     ;;
    (dolist (tl trainList)

      (setq inputs (car tl))
      (setq targetOutputs (cadr tl))

      (if *delta-rule-debug-flag*
        (print (list "Current targets:" targetOutputs)))

      (setq iDimension (car sizeList)) ; get the size of the input slab
      (setq iActivationVector (car activationList)) ; get array of input activations
      (dotimes (i iDimension) ; copy training inputs to input slab
        (setf
         (aref iActivationVector i)
         (+ (nth i inputs) (frandom (- inputNoise) inputNoise))))
      ;;
       ; Propagate activation through all of the slabs:
       ;;
      (dotimes (n-1 (- nLayers 1))  ; update layer i to layer flowing to layer j
        (setq n (+ n-1 1))
        (setq jDimension (nth n sizeList)) ; get the size of the j'th layer
        (setq jActivationVector (nth n activationList)) ; activation array for slab j
        (setq weightArray (nth n-1 weightList))
        (setq sumOfProductsArray (nth n-1 sumOfProductsList))
        (dotimes (j jDimension) ; process each neuron in slab j
          (setq sum 0.0) ; init sum of products to zero
          (dotimes (i iDimension) ; to get activation from each neuron in previous slab
            (setq
             sum
             (+ sum (* (aref weightArray i j) (aref iActivationVector i)))))
          (setf (aref sumOfProductsArray j) sum) ; save sum of products
          (setf (aref jActivationVector j) (Sigmoid sum)))
        (setq iDimension jDimension)     ; reset index for next slab pair
        (setq iActivationVector jActivationVector))
      ;;
       ; Activation is  spread through the network and sum of products calculated.
       ; Now modify the weights in the network using back error propagation. Start
       ; by calculating the error signal for each neuron in the output layer:
       ;;
      (setq jDimension (nth (- nLayers 1) sizeList)) ; size of last layer
      (setq jActivationVector (nth (- nLayers 1) activationList))
      (setq jDeltaVector (nth (- nLayers 2) deltaList))
      (setq sumOfProductsArray (nth (- nLayers 2) sumOfProductsList))
      (setq outputError 0)
      (dotimes (j jDimension)
        (setq delta (- (nth j targetOutputs) (aref jActivationVector j)))
        (setq outputError (+ outputError (abs delta)))
        (setf
         (aref jDeltaVector j)
         (+
          (aref jDeltaVector j)
          (* delta (dSigmoid (aref sumOfProductsArray j))))))
      ;;
       ; Now calculate the backpropagated error signal for all hidden slabs:
       ;;
      (dotimes (nn (- nLayers 2))
        (setq n (- nLayers 3 nn))
        (setq iDimension (nth (+ n 1) sizeList))
        (setq iSumOfProductsArray (nth n sumOfProductsList))
        (setq iDeltaVector (nth n deltaList))
        (dotimes (i iDimension)
          (setf (aref iDeltaVector i) 0.0))
        (setq weightArray (nth (+ n 1) weightList))
        (dotimes (i iDimension)
          (setq error 0.0)
          (dotimes (j jDimension)
            (setq error (+ error (* (aref jDeltaVector j) (aref weightArray i j)))))
          (setf
           (aref iDeltaVector i)
           (+
            (aref iDeltaVector i)
            (* error (dSigmoid (aref iSumOfProductsArray i))))))
        (setq jDimension iDimension)
        (setq jDeltaVector iDeltaVector))

      ;;
       ; Update all delta weights in the network:
       ;;
      (setq iDimension (car sizeList))
      (dotimes (n (- nLayers 1))
        (setq iActivationVector (nth n activationList))
        (setq jDimension (nth (+ n 1) sizeList))
        (setq jDeltaVector (nth n deltaList))
        (setq deltaWeightArray (nth n deltaWeightList))
        (setq weightArray (nth n weightList))
        (setq eida (nth n eidaList))

        (dotimes (j jDimension)
          (dotimes (i iDimension)
            (setq delta (* eida (aref jDeltaVector j) (aref iActivationVector i)))
            (setf
             (aref DeltaWeightArray i j)
             (+ (aref DeltaWeightArray i j) delta)))) ; remember delta weight change

        (setq iDimension jDimension))

    ;;
     ; Update all weights in the network:
     ;;
    (setq iDimension (car sizeList))
    (dotimes (n (- nLayers 1))
      (setq iActivationVector (nth n activationList))
      (setq jDimension (nth (+ n 1) sizeList))
      (setq jDeltaVector (nth n deltaList))
      (setq deltaWeightArray (nth n deltaWeightList))
      (setq oldDeltaWeightArray (nth n oldDeltaWeightList))
      (setq weightArray (nth n weightList))
      (dotimes (j jDimension)
        (dotimes (i iDimension)
          (setf
           (aref weightArray i j)
           (+ (aref weightArray i j)
              (* alpha (aref deltaWeightArray i j))
              (* beta  (aref oldDeltaWeightArray i j))))
           (setf (aref oldDeltaWeightArray i j) ; save current delta weights
                 (aref deltaWeightArray i j)))) ; ...for next momentum term.
      (setq iDimension jDimension))

    (if plot-output-file-name
        (DeltaPlot netList plot-output-file-name)))

    (/ outputError jDimension)))

;;
 ;  Utility for using a trained neural network in the recall mode.
 ;  The first argument to this function is a network definition (as
 ;  returned from NewDeltaNetwork) and the second argument is a list
 ;  of input neuron activation values to drive through the network.
 ;;
(defun DeltaRecall (netList inputs)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (weightList (cadr (cdddr netList)))
        (iDimension nil)
        (jDimension nil)
        (iActivationVector nil)
        (jActivationVector nil)
        (n nil)
        (weightArray nil)
        (returnList nil)
        (sum nil))
    (setq iDimension (car sizeList)) ; get the size of the input slab
    (setq iActivationVector (car activationList)) ; get array of input activations
    (dotimes (i iDimension) ; copy training inputs to input slab
      (setf (aref iActivationVector i) (nth i inputs)))
    (dotimes (n-1 (- nLayers 1))  ; update layer j to layer i
      (setq n (+ n-1 1))
      (setq jDimension (nth n sizeList)) ; get the size of the j'th layer
      (setq jActivationVector (nth n activationList)) ; activation array for slab j
      (setq weightArray (nth n-1 weightList))
      (dotimes (j jDimension) ; process each neuron in slab j
        (setq sum 0.0) ; init sum of products to zero
        (dotimes (i iDimension) ; to get activation from each neuron in previous slab
          (setq
           sum
           (+ sum (* (aref weightArray i j) (aref iActivationVector i)))))
        (if *delta-rule-debug-flag*
          (print (list "sum=" sum)))
        (setf (aref jActivationVector j) (Sigmoid sum)))
      (setq iDimension jDimension) ; get ready for next slab pair
      (setq iActivationVector jActivationVector))
      (dotimes (j jDimension)
        (setq returnList (append returnList (list (aref jActivationVector j)))))
      returnList))

;;
 ; Throw away test functions for two, three and four layer networks:
 ;;

(proclaim '(special temp))  ; temp will be used to hold the network data

(defun test2 (&aux RMSerror temp)
  (setq temp (newdeltanetwork '(2 2))) ; specify a two layer network (2x2)
  (dotimes (ii 1000)
    (let ((file-name
     (if (equal (mod ii 150) 0)
         (concatenate 'string "output_plot_" (format nil "~12,'0d" ii) ".png")
         nil)))
      (setq
       RMSerror
       (deltalearn
  file-name temp
  '(((1 0) (0 1))
    ((0 1) (1 0)))))
      (if (equal (mod ii 50) 0) ;; print out every 50 cycles
    (progn
      (princ "....training cycle \#")
      (princ ii)
      (princ " RMS error = ")
      (princ RMSerror)
      (terpri))))))

(defun test3 (&optional (restart 'yes) &aux RMSerror) ; three layer network
  (if
    (equal restart 'yes)
    (setq temp (newdeltanetwork '(5 4 5))))
  (dotimes (ii 3000)
    (let ((file-name
     (if (equal (mod ii 400) 0)
         (concatenate 'string "output_plot_" (format nil "~12,'0d" ii) ".png")
         nil)))
      (setq
       RMSerror
       (deltalearn
  file-name temp
  '(((1 0 0 0 0) (0 1 0 0 0))
    ((0 1 0 0 0) (0 0 1 0 0))
    ((0 0 1 0 0) (0 0 0 1 0))
    ((0 0 0 1 0) (0 0 0 0 1))
    ((0 0 0 0 1) (1 0 0 0 0)))))
      (if (equal (mod ii 50) 0) ;; print out every 50 cycles
    (progn
      (princ "....training cycle \#")
      (princ ii)
      (princ " RMS error = ")
      (princ RMSerror)
      (terpri))))))

(defun test4 (&optional (restart 'yes) &aux RMSerror)  ; four layer network
  (if
    (equal restart 'yes)
    (setq temp (newdeltanetwork '(4 5 5 4))))
  (dotimes (ii 7000)
    (let ((file-name
     (if (equal (mod ii 1800) 0)
         (concatenate 'string "output_plot_" (format nil "~12,'0d" ii) ".png")
         nil)))
      (setq
       RMSerror
       (deltalearn
  file-name
  temp
  '(((1 0 0 0) (0 1 0 0))
    ((0 1 0 0) (0 0 1 0))
    ((0 0 1 0) (0 0 0 1))
    ((0 0 0 1) (1 0 0 0)))))
      (if (equal (mod ii 150) 0) ;; print out every 150 cycles
    (progn
      (princ "....training cycle \#")
      (princ ii)
      (princ " RMS error = ")
      (princ RMSerror)
      (terpri))))))

(princ "There are 3 test functions for 2, 3, and 4 layered backprop models. Try:")
(terpri)
(princ "(in-package #:deltarule)")
(terpri)
(princ "(test2)")
(terpri)
(princ "(test3)")
(terpri)
(princ "(test4)")
(terpri)
