;;
 ;  Backpropagation neural network example from the chapter
 ;  "Backpropagation Neural Networks". Only the neural network code
 ;  from the chapter listings is included here (no plotting code).
 ;;

(defparameter *delta-rule-debug-flag* nil)

(defparameter *delta-default-input-noise-value* 0.0)

(defun frandom (min max)
  (+ min (* (random 1.0) (- max min))))

(defun Sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun dSigmoid (x)
  (let ((temp (Sigmoid x)))
    (* temp (- 1.0 temp))))

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
;
(defun NewDeltaNetwork (sizeList
                        &aux (numLayers (length sizeList))
                             (alpha 0.5)
                             (beta 0.25)
                             a-list s-list w-list dw-list d-list old-dw-list)

    ;;
     ;  Allocate storage for activations, sum of products, back
     ;  propagation errors, and weights:
     ;;
    (dotimes (n numLayers)
      (push (make-array (nth n sizeList)
                        :initial-element 0.0)
            a-list))
    (setq a-list (reverse a-list))

    (dotimes (n (- numLayers 1))
      (push (make-array (nth (+ n 1) sizeList)
                        :initial-element 0.0)
            s-list))
    (setq s-list (reverse s-list))

    (dotimes (n (- numLayers 1))
      (push (make-array (nth (+ n 1) sizeList)
                        :initial-element 0.0)
            d-list))
    (setq d-list (reverse d-list))

    (dotimes (n (- numLayers 1))
      (push (make-array (list (nth n sizeList) (nth (+ n 1) sizeList))
                        :initial-element 0.0)
            w-list))
    (setq w-list (reverse w-list))

    (dotimes (n (- numLayers 1))
      (push (make-array (list (nth n sizeList) (nth (+ n 1) sizeList))
                        :initial-element 0.0)
            dw-list))
    (setq dw-list (reverse dw-list))

    (dotimes (n (- numLayers 1))
      (push (make-array (list (nth n sizeList) (nth (+ n 1) sizeList))
                        :initial-element 0.0)
            old-dw-list))
    (setq old-dw-list (reverse old-dw-list))

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
          d-list old-dw-list alpha beta))

;;
 ;  Utility function for training a delta rule neural network.
 ;  The first argument is a network definition (as returned from
 ;  NewDeltaNetwork), the second argument is a list of training
 ;  data cases (see the example test functions at the end of this
 ;  file for examples.
 ;;

(defun DeltaLearn (netList trainList)
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
        (eidaList nil)
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
     ;  Per-layer learning rates used when accumulating delta weights:
     ;;
    (setq eidaList (make-list (- nLayers 1) :initial-element alpha))

    ;;
     ;  Main loop on training examples:
     ;;
    (dolist (tl trainList)

      (setq inputs (car tl))
      (setq targetOutputs (cadr tl))

      (if *delta-rule-debug-flag*
        (print (list "Current targets:" targetOutputs)))

      (setq iDimension (car sizeList)) ; get the size of the input slab
      (setq iActivationVector (car activationList)) ; input activations
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
        (setq jActivationVector (nth n activationList)) ; activation  for slab j
        (setq weightArray (nth n-1 weightList))
        (setq sumOfProductsArray (nth n-1 sumOfProductsList))
        (dotimes (j jDimension) ; process each neuron in slab j
          (setq sum 0.0) ; init sum of products to zero
          (dotimes (i iDimension) ; activation from neurons in previous slab
            (setq
             sum
             (+ sum (* (aref weightArray i j) (aref iActivationVector i)))))
          (setf (aref sumOfProductsArray j) sum) ; save sum of products
          (setf (aref jActivationVector j) (Sigmoid sum)))
        (setq iDimension jDimension)     ; reset index for next slab pair
        (setq iActivationVector jActivationVector))
      ;;
       ; Activation is  spread through the network and sum of products
       ; calculated. Now modify the weights in the network using back
       ; error propagation. Start by calculating the error signal for
       ; each neuron in the output layer:
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
            (setq error
                  (+ error (* (aref jDeltaVector j) (aref weightArray i j)))))
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
             (+ (aref DeltaWeightArray i j) delta)))) ; delta weight changes

        (setq iDimension jDimension)))

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

    (/ outputError jDimension)))

;;
 ;  Utility for using a trained neural network in the recall mode.
 ;  The first argument to this function is a network definition (as
 ;  returned from NewDeltaNetwork) and the second argument is a list
 ;  of input neuron activation values to drive through the network.
 ;  The output is a list of the calculated activation energy for
 ;  each output neuron.
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
    (setq iActivationVector (car activationList)) ; get input activations
    (dotimes (i iDimension) ; copy training inputs to input slab
      (setf (aref iActivationVector i) (nth i inputs)))
    (dotimes (n-1 (- nLayers 1))  ; update layer j to layer i
      (setq n (+ n-1 1))
      (setq jDimension (nth n sizeList)) ; get the size of the j'th layer
      (setq jActivationVector (nth n activationList)) ; activation for slab j
      (setq weightArray (nth n-1 weightList))
      (dotimes (j jDimension) ; process each neuron in slab j
        (setq sum 0.0) ; init sum of products to zero
        (dotimes (i iDimension) ; get activation from each neuron in last slab
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

(defun test3 (&optional (restart 'yes) &aux RMSerror (temp nil)) ; three layer network
  (if
    (equal restart 'yes)
    (setq temp (newdeltanetwork '(5 4 5))))
  (dotimes (ii 3000)
    (setq
     RMSerror
     (deltalearn
        temp
        '(((1 0 0 0 0) (0 1 0 0 0))
          ((0 1 0 0 0) (0 0 1 0 0))
          ((0 0 1 0 0) (0 0 0 1 0))
          ((0 0 0 1 0) (0 0 0 0 1))
          ((0 0 0 0 1) (1 0 0 0 0)))))
    (if (equal (mod ii 50) 0) ;; print error out every 50 cycles
      (progn
        (princ "....training cycle \#")
        (princ ii)
        (princ " RMS error = ")
        (princ RMSerror)
        (terpri)))))
