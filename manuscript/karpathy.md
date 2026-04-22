# Building a MicroGPT in Common Lisp

In the next several chapters we will use production-grade Large Language Models like GPT-5, Gemini, Claude, etc. Here we look at optional material: Karpathy’s MicroGPT re-implemented in Common Lisp.

## Introduction

In recent years, the artificial intelligence landscape has been completely transformed by large language models (LLMs) built upon the foundational Transformer architecture. Models like GPT-4, LLaMA, Gemma, and Claude contain tens to hundreds of billions of parameters. Their capabilities are profound, ranging from generating sophisticated poetry to writing complex computer code. However, the sheer size and complexity of these production models can make them feel like impenetrable black boxes to developers and researchers trying to grasp their inner workings.

To understand how these massive systems operate, it can be beneficial to strip away the complexities of distributed training, massive datasets, and heavily optimized C++/CUDA backend libraries. One of the most famous educational projects in this domain is Andrej Karpathy's MicroGPT (often associated with his broader `nanoGPT` and `micrograd` repositories). Karpathy's work distills the essence of a Generative Pre-trained Transformer into a bare-bones, dependency-free implementation, allowing anyone to trace the mathematical operations from start to finish.

In this chapter, we will explore a complete, dependency-free port of MicroGPT written entirely in Common Lisp. Common Lisp has a rich, storied history in the field of artificial intelligence. Its symbolic nature, dynamic typing, and interactive REPL-driven development cycle make it a uniquely powerful tool for exploring complex algorithmic architectures. By stepping away from the heavy machinery of modern Python frameworks like PyTorch, TensorFlow, or JAX, we are forced to construct the foundational components from scratch. We will build an automatic differentiation engine (autograd), construct neural network layers, implement the multi-head self-attention mechanism, and design the training loop. This hands-on approach not only demystifies how modern AI systems learn but also beautifully demonstrates the expressive power and mathematical elegance of Common Lisp.

## Demystifying the Core Components

To build a functioning, end-to-end language model from scratch, we need to orchestrate several interacting systems. Our Common Lisp implementation elegantly encapsulates these systems into a single, cohesive file. Let us break down the architecture into its primary constituents: the autograd engine, model parameter initialization, the Transformer architecture, the training loop, and the inference generator. Understanding each of these pieces is crucial for grasping the holistic behavior of a language model.

### The Autograd Engine

At the heart of any modern neural network training process is the ability to automatically compute the gradients of the loss function with respect to the model's millions (or in this case, hundreds) of parameters. This is achieved through a technique known as reverse-mode automatic differentiation. 

In our Lisp code, this mechanism is implemented using a simple `defstruct` called `value`. A `value` structure encapsulates a scalar piece of data, its current gradient, the child nodes from which it was computed, and the local gradients with respect to those children. This means that every time we perform a mathematical operation, we are not merely calculating a result; we are dynamically building a computational graph.

When operations like addition (`v+`), multiplication (`v*`), exponentiation (`vpow`), or non-linearities like ReLU (`vrelu`) are executed, they instantiate new `value` objects. These objects maintain pointers to their operands. For example, if `c = a + b`, then `c` knows that it was created from `a` and `b`, and it stores the local derivatives of the addition operation.

Once a forward pass through the network is complete and a scalar loss value is computed (representing how wrong the network's predictions were), the `backward` function takes over. It performs a topological sort of the entire computational graph. Then, traversing this sorted list in reverse order, it applies the chain rule from calculus to propagate gradients backward from the loss down to every individual weight of the model. This dependency-free approach, heavily inspired by Karpathy's `micrograd`, clearly illustrates that automatic differentiation is fundamentally just an automated, programmatic application of the chain rule over a directed acyclic graph.

### Model Parameters and Initialization

A neural network is essentially an enormous collection of parameters—weights and biases—that are iteratively optimized over time. In our Lisp implementation, these parameters are stored in a centralized hash table called `*state-dict*`. 

The `init-model` function defines the architectural shape of our MicroGPT. We initialize several distinct sets of matrices:
- **`wte`**: The token embedding matrix, which translates discrete character IDs into dense continuous vectors.
- **`wpe`**: The positional embedding matrix, which gives the model information about where a token is located in the sequence.
- **`lm_head`**: The final language modeling head, a linear layer that projects the internal representations back into the vocabulary space to predict the next character.
- **Transformer Layers**: For each layer, we initialize matrices for the attention mechanism (`attn_wq`, `attn_wk`, `attn_wv`, `attn_wo`) and the multi-layer perceptron (`mlp_fc1`, `mlp_fc2`).

Each parameter is initialized using a Gaussian distribution (`random-gauss`). This small amount of random noise provides the network with an asymmetrical starting state, breaking symmetry and allowing gradient descent to effectively navigate the loss landscape. Crucially, every weight generated is a `value` structure, ensuring that every calculation utilizing these weights is automatically tracked by the autograd engine.

### The Transformer Architecture

The `gpt` function serves as the central processing unit of the model. It implements the forward pass of a decoder-only Transformer. As data flows through this function, it undergoes a series of sophisticated transformations:

1. **Embeddings:** Each discrete token (in our case, characters) and its position in the sequence are converted into dense vectors using our embedding matrices. These two vectors are added together to form the initial representation of the input sequence.
2. **Layer Normalization:** We employ RMSNorm (Root Mean Square Normalization) to stabilize the neural activations. RMSNorm is computationally simpler than standard Layer Normalization because it does not require mean-centering, yet it is highly effective. It has become a standard technique in modern, highly optimized models like Meta's LLaMA.
3. **Multi-Head Self-Attention:** This is the defining feature of the Transformer. The model linearly projects the input into three distinct representations: Queries (Q), Keys (K), and Values (V). For each attention head, the model computes the dot product of the Query and Key. This dot product determines how much "attention" the current token should pay to all past tokens. The attention scores are normalized using a `vsoftmax` function and are then multiplied by the Value vector. To significantly speed up inference, we maintain a Key-Value (KV) cache. This cache stores past K and V computations, preventing the model from redundantly recalculating past context for every newly generated token.
4. **Multi-Layer Perceptron (MLP):** Following the attention mechanism, the data is passed through a simple feed-forward neural network equipped with a ReLU activation function. While the attention mechanism allows tokens to communicate with one another, the MLP allows each individual token to process and refine the information it has gathered.
5. **Residual Connections:** You will notice in the code that the outputs of the attention layer and the MLP layer are added back to their respective inputs (`setf x (mapcar #'v+ x xr)`). These residual connections create "shortcuts" for gradients to flow backward through the network, mitigating the vanishing gradient problem and enabling the training of deeper architectures.
6. **Language Modeling Head:** Finally, the refined representations are projected back into the vocabulary space. The output is a set of raw scores, or "logits," representing the model's prediction for what the next token in the sequence should be.

### Training the Model: Learning from Data

The `run-training` function orchestrates the intricate dance of the learning process. The model trains by reading lines of text—in our specific example, from a file called `names.txt`—processing it character by character. 

The training loop proceeds iteratively over many steps:
1. **The Forward Pass:** The model ingests a sequence of characters and attempts to predict the subsequent character for every position in the sequence simultaneously.
2. **Loss Calculation:** We utilize the cross-entropy loss function. By applying the softmax function to our raw logits, we convert them into a normalized probability distribution. The loss is calculated as the negative log-likelihood of the correct next character. If the model assigns a high probability to the correct character, the loss is low. If it assigns a low probability, the loss is high.
3. **The Backward Pass:** The `backward` function is invoked on the final scalar loss value. This single function call ripples backward through the computational graph, computing the exact gradient for every parameter in the network.
4. **Optimization:** We update the parameters using the Adam optimization algorithm. Unlike simple stochastic gradient descent, Adam maintains running averages of both the gradients and the squared gradients. This allows it to adaptively tune the learning rate for each individual parameter, resulting in faster and more stable convergence.

By executing this loop over hundreds or thousands of steps, the model incrementally adjusts its internal weights. It slowly learns to assign higher probabilities to the correct sequences of characters, thereby absorbing the underlying statistical structure of the training data.

### Inference: Hallucinating New Data

Once the model has completed its training phase and internalized the patterns of the dataset, we can use it to generate novel text via the `run-inference` function. 

Starting with a special Beginning-Of-Sequence (BOS) token, we feed this initial context into the model to obtain the probabilities for the first character. We then sample from this probability distribution (using the `random-choice` function) to select a character. This newly generated character is appended to our context and fed back into the model to predict the *next* character. This autoregressive generation process continues in a loop until the model predicts the BOS token again, which we use to indicate the end of the sequence.

Because we sample from a probability distribution rather than greedily picking the single most likely token at every step, the model can generate diverse, creative, and sometimes surprising outputs. It effectively "hallucinates" new names or words that strongly resemble the stylistic patterns of the training data but were never explicitly contained within it.

## Conclusion

This complete Common Lisp implementation of MicroGPT vividly demonstrates that the core concepts underpinning modern large language models are not inscrutable magic. By methodically breaking down the architecture into its fundamental mathematical operations and constructing a rudimentary but fully functional autograd engine from scratch, we gain a profound, demystified understanding of how these powerful systems learn and operate. 

Common Lisp, with its unparalleled flexibility and expressive syntax, proves to be an exceptionally capable and elegant language for this endeavor. Its interactive nature makes it a joy to use for defining complex computational graphs and experimenting with novel neural network architectures.

In the following section, you will find the complete source code for our dependency-free MicroGPT. Reading carefully through this program listing will help solidify the theoretical concepts discussed in this chapter and provide you with a robust, transparent foundation for further experimentation and exploration in the fascinating world of language modeling.

## Complete Source Code Listing for `microgpt.lisp`

```lisp
;;;; microgpt.lisp — Karpathy's microGPT in dependency-free Common Lisp

(defpackage #:microgpt (:use #:cl) (:export #:run-microgpt))
(in-package #:microgpt)

(defun random-gauss (mean std)
  (+ mean (* std (sqrt (* -2 (log (max (random 1.0) 1e-15)))) (cos (* 2 pi (random 1.0))))))

(defun shuffle-list (seq)
  (let ((v (coerce seq 'vector)))
    (loop for i from (1- (length v)) downto 1 for j = (random (1+ i))
          do (rotatef (aref v i) (aref v j)))
    (coerce v 'list)))

(defun read-lines (file)
  (with-open-file (s file :if-does-not-exist nil)
    (when s
      (loop for line = (read-line s nil) while line
            for tr = (string-trim '(#\Space #\Tab #\Newline #\Return) line)
            when (plusp (length tr)) collect tr))))

(defparameter *docs* nil) (defparameter *uchars* nil) (defparameter *bos* nil)
(defparameter *vocab-size* nil) (defparameter *n-layer* 1) (defparameter *n-embd* 16)
(defparameter *block-size* 16) (defparameter *n-head* 4) (defparameter *head-dim* 4)
(defparameter *state-dict* (make-hash-table :test 'equal)) (defparameter *params* nil)

;;; Autograd
(defstruct (value (:print-function print-value) (:constructor %make-value))
  (data 0.0 :type single-float) (grad 0.0 :type single-float)
  (children nil) (local-grads nil))

(defun print-value (v s d) (declare (ignore d))
  (format s "<Value ~A/~A>" (value-data v) (value-grad v)))

(defun ensure (v) (if (value-p v) v (%make-value :data (float v 1.0))))
(defun make-value (d &optional ch lg)
  (%make-value :data (if (value-p d) (value-data d) (float d 1.0)) :children ch :local-grads lg))

(defun v+ (a b)
  (let ((a (ensure a)) (b (ensure b)))
    (make-value (+ (value-data a) (value-data b)) (list a b) '(1.0 1.0))))
(defun v* (a b)
  (let ((a (ensure a)) (b (ensure b)))
    (make-value (* (value-data a) (value-data b)) (list a b) (list (value-data b) (value-data a)))))
(defun vpow (v p)
  (let ((v (ensure v)))
    (make-value (expt (value-data v) p) (list v)
                (list (coerce (* p (expt (value-data v) (1- p))) 'single-float)))))
(defun vlog (v) (let ((v (ensure v))) (make-value (log  (value-data v)) (list v) (list (/ 1.0 (value-data v))))))
(defun vexp (v) (let ((v (ensure v))) (make-value (exp  (value-data v)) (list v) (list (exp (value-data v))))))
(defun vrelu (v) (let ((v (ensure v))) (make-value (max 0.0 (value-data v)) (list v) (list (if (plusp (value-data v)) 1.0 0.0)))))
(defun vneg (v) (v* v -1.0))
(defun v- (a b) (v+ a (vneg b)))
(defun v/ (a b) (v* a (vpow b -1.0)))

(defun backward (v)
  (let ((topo nil) (seen (make-hash-table :test 'eq)))
    (labels ((build (n)
               (unless (gethash n seen)
                 (setf (gethash n seen) t)
                 (mapc #'build (value-children n))
                 (push n topo))))
      (build v))
    (setf (value-grad v) 1.0)
    (dolist (n topo)
      (mapc (lambda (c g) (incf (value-grad c) (* g (value-grad n))))
            (value-children n) (value-local-grads n)))))

;;; Model Parameters
(defun make-matrix (nout nin &optional (std 0.08))
  (coerce (loop for i below nout collect
                (coerce (loop for j below nin collect
                              (let ((v (make-value (random-gauss 0 std))))
                                (push v *params*) v))
                        'vector))
          'vector))

(defun init-model ()
  (setf *state-dict* (make-hash-table :test 'equal) *params* nil)
  (flet ((mat (k r c) (setf (gethash k *state-dict*) (make-matrix r c))))
    (mat "wte" *vocab-size* *n-embd*) (mat "wpe" *block-size* *n-embd*) (mat "lm_head" *vocab-size* *n-embd*)
    (dotimes (i *n-layer*)
      (flet ((lmat (s r c) (mat (format nil "layer~A.~A" i s) r c)))
        (lmat "attn_wq" *n-embd* *n-embd*) (lmat "attn_wk" *n-embd* *n-embd*)
        (lmat "attn_wv" *n-embd* *n-embd*) (lmat "attn_wo" *n-embd* *n-embd*)
        (lmat "mlp_fc1" (* 4 *n-embd*) *n-embd*) (lmat "mlp_fc2" *n-embd* (* 4 *n-embd*)))))
  (setf *params* (nreverse *params*)))

;;; Architecture
(defun vsum (vs) (reduce #'v+ vs :initial-value (make-value 0.0)))
(defun vlinear (x w)
  (loop for row across w
        collect (reduce #'v+ (mapcar #'v* (coerce row 'list) x) :initial-value (make-value 0.0))))
(defun vsoftmax (logits)
  (let* ((mx (reduce #'max logits :key #'value-data))
         (exps (mapcar (lambda (v) (vexp (v- v mx))) logits))
         (sum (vsum exps)))
    (mapcar (lambda (e) (v/ e sum)) exps)))
(defun vrmsnorm (x)
  (let ((sc (vpow (v+ (v/ (vsum (mapcar (lambda (xi) (v* xi xi)) x)) (length x)) 1e-5) -0.5)))
    (mapcar (lambda (xi) (v* xi sc)) x)))
(defun make-kv-cache ()
  (coerce (loop for i below *n-layer* collect (make-array 0 :fill-pointer 0 :adjustable t)) 'vector))

(defun gpt (tok-id pos-id keys vals)
  (let ((x (mapcar #'v+ (coerce (aref (gethash "wte" *state-dict*) tok-id) 'list)
                        (coerce (aref (gethash "wpe" *state-dict*) pos-id) 'list))))
    (setf x (vrmsnorm x))
    (dotimes (li *n-layer*)
      (flet ((sd (s) (gethash (format nil "layer~A.~A" li s) *state-dict*)))
        (let ((xr x))
          (setf x (vrmsnorm x))
          (let* ((q (vlinear x (sd "attn_wq")))
                 (k (vlinear x (sd "attn_wk")))
                 (v (vlinear x (sd "attn_wv"))))
            (vector-push-extend k (aref keys li))
            (vector-push-extend v (aref vals li))
            (setf x (vlinear
                     (loop for h below *n-head* nconc
                           (let* ((hs (* h *head-dim*)) (he (+ hs *head-dim*))
                                  (q-h (subseq q hs he))
                                  (k-h (map 'list (lambda (ki) (subseq ki hs he)) (aref keys li)))
                                  (v-h (map 'list (lambda (vi) (subseq vi hs he)) (aref vals li)))
                                  (aw (vsoftmax (loop for kt in k-h collect
                                                      (v/ (vsum (mapcar #'v* q-h kt)) (sqrt *head-dim*))))))
                             (loop for j below *head-dim* collect
                                   (vsum (mapcar (lambda (w vt) (v* w (nth j vt))) aw v-h)))))
                     (sd "attn_wo")))
            (setf x (mapcar #'v+ x xr))))
        (let ((xr x))
          (setf x (vrmsnorm x))
          (setf x (mapcar #'vrelu (vlinear x (sd "mlp_fc1"))))
          (setf x (vlinear x (sd "mlp_fc2")))
          (setf x (mapcar #'v+ x xr)))))
    (vlinear x (gethash "lm_head" *state-dict*))))

;;; Training
(defun run-training ()
  (let* ((lr 0.01) (b1 0.85) (b2 0.99) (eps 1e-8) (steps 1000) (np (length *params*))
         (m (make-array np :initial-element 0.0)) (mv (make-array np :initial-element 0.0)))
    (dotimes (step steps)
      (let* ((doc (nth (mod step (length *docs*)) *docs*))
             (tokens (append (list *bos*)
                             (loop for ch across doc collect (position ch *uchars*))
                             (list *bos*)))
             (n (min *block-size* (1- (length tokens))))
             (keys (make-kv-cache)) (vals (make-kv-cache)) (losses nil))
        (dotimes (pos n)
          (let* ((logits (gpt (nth pos tokens) pos keys vals))
                 (probs (coerce (vsoftmax logits) 'vector)))
            (push (vneg (vlog (aref probs (nth (1+ pos) tokens)))) losses)))
        (let* ((loss (v/ (vsum losses) n)) (t+1 (1+ step)))
          (backward loss)
          (loop for i below np for p in *params* do
                (setf (aref m i) (+ (* b1 (aref m i)) (* (- 1 b1) (value-grad p))))
                (setf (aref mv i) (+ (* b2 (aref mv i)) (* (- 1 b2) (expt (value-grad p) 2))))
                (let ((mh (/ (aref m i) (- 1 (expt b1 t+1))))
                      (vh (/ (aref mv i) (- 1 (expt b2 t+1)))))
                  (decf (value-data p) (/ (* lr (- 1 (/ step steps)) mh) (+ (sqrt vh) eps)))
                  (setf (value-grad p) 0.0)))
          (format t "step ~4D/~4D | loss ~,4F~%" t+1 steps (value-data loss))
          (finish-output))))))

;;; Inference
(defun random-choice (weights)
  (let ((r (random (reduce #'+ weights))) (acc 0.0))
    (dotimes (i (1- (length weights)) (1- (length weights)))
      (incf acc (aref weights i))
      (when (< r acc) (return-from random-choice i)))))

(defun run-inference ()
  (format t "~%--- inference (new, hallucinated names) ---~%")
  (dotimes (i 20)
    (let ((keys (make-kv-cache)) (vals (make-kv-cache)) (tok *bos*) (sample nil))
      (dotimes (pos *block-size*)
        (let* ((lt (mapcar (lambda (l) (/ (value-data l) 0.5)) (gpt tok pos keys vals)))
               (mx (reduce #'max lt))
               (exps (mapcar (lambda (v) (exp (- v mx))) lt))
               (probs (coerce (mapcar (lambda (e) (/ e (reduce #'+ exps))) exps) 'vector)))
          (setf tok (random-choice probs))
          (if (= tok *bos*) (return) (push (char *uchars* tok) sample))))
      (format t "sample ~2D: ~A~%" (1+ i) (coerce (reverse sample) 'string)))))

;;; Main
(defun run-microgpt ()
  (setf *random-state* (make-random-state t)
        *docs* (shuffle-list (read-lines "names.txt")))
  (format t "num docs: ~A~%" (length *docs*))
  (let ((chars nil))
    (dolist (doc *docs*) (loop for ch across doc do (pushnew ch chars)))
    (setf *uchars* (coerce (sort chars #'char<) 'string)
          *bos* (length *uchars*)
          *vocab-size* (1+ *bos*)))
  (format t "vocab size: ~A~%" *vocab-size*)
  (init-model)
  (format t "num params: ~A~%" (length *params*))
  (run-training)
  (run-inference))

(microgpt:run-microgpt)
```

Let’s run the code:

```bash
$ sbcl
* (load "microgpt.lisp")
num docs: 32033
vocab size: 27
num params: 4192
step    1/1000 | loss 3.3421
step    2/1000 | loss 3.5779
...
step  997/1000 | loss 2.2916
step  998/1000 | loss 1.7724
step  999/1000 | loss 2.3279
step 1000/1000 | loss 2.3213

--- inference (new, hallucinated names) ---
sample  1: lereri
sample  2: nahand
sample  3: eleni
sample  4: amica
sample  5: calina
sample  6: alla
sample  7: marona
sample  8: chidynn
sample  9: kanan
sample 10: brann
sample 11: kalen
sample 12: jorena
```


## Wrap Up

This concludes our exploration of MicroGPT implemented from scratch in Common Lisp. By studying the complete source code listing provided above, you have seen firsthand how the abstract mathematical concepts of reverse-mode automatic differentiation, self-attention, and autoregressive language modeling translate into concrete, functional code.

I highly encourage you to load `microgpt.lisp` into your own Common Lisp REPL and run it. Try experimenting with the hyperparameters—adjust the `*n-layer*`, `*n-embd*`, or `*n-head*` variables and observe how they impact both the training time and the quality of the hallucinated outputs. You can also swap out the `names.txt` dataset for your own text files to see how the model adapts to entirely different linguistic patterns. By actively tinkering with this minimalistic implementation, you will cement your understanding of how modern generative AI truly works under the hood.
