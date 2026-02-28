# Reinforcement Learning

The field of Reinforcement Learning is a "large tent" that includes many techniques. I wrote this chapter after taking the excellent Reinforcement Learning specialization that is comprised of four classes at [Cousera](https://www.coursera.org/specializations/reinforcement-learning) taught by Martha White and Adam White at the University of Alberta. This class uses the standard text by Richard Sutton and Andrew Barto; the book and other materials [are available here](http://incompleteideas.net/book/the-book.html) as a free PDF download, sample software in Python and Common Lisp, and links to buy the physical book. While I have made the examples in this chapter self-contained, if this material interests you then take the classed read the book by Sutton and Barto.

## Required Software

You need to install **magicl** and you have choices to make. You can just install the pure Common Lisp core or install optional high performance backends like BLAS and LAPACK.

I assume that you, dear reader, have some familiarity with linear algebra. I recommend taking a few minutes to view the big level **magicl** APIs: [read through this right now](https://github.com/rigetti/magicl/blob/master/doc/high-level.md).

### Install magicl with BLAS and LAPACK Back Ends

I recommend that you just install the core pure Common Lisp library by following these instructions:

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/rigetticomputing/magicl.git
cd magicl
```

Edit the file **magicl.asd** and delete all lines in the file below:

```
;;; EXPOKIT
```

Then comment out the line for ExpoKit:

```
:depends-on (#:magicl/core
             #:magicl/ext-blas
             #:magicl/ext-lapack
             ;;#:magicl/ext-expokit
            ))
```

Then you can compile and use **magicl** using Quicklisp:

```
$ sbcl
* (quicklisp:quickload "magicl")
To load "magicl":
  Load 1 ASDF system:
    magicl
; Loading "magicl"
............
("magicl")
* (defvar m1 (rand '(4 4)))
m1
* (defvar m2 (rand '(4 2)))
m2
* (@ m1 m2) ;; multiply
#<matrix/double-float (4x2):
   0.302     0.376
   1.292     1.465
   1.519     1.599
   1.552     1.561>
* (det m1).  ;; determinant
-0.00774559430671582d0
* (trace m1)
1.9824416224540566d0
* (upper-triangular m1)
#<matrix/double-float (4x4):
   0.238     0.171     0.012     0.094
   0.000     0.448     0.398     0.277
   0.000     0.000     0.782     0.299
   0.000     0.000     0.000     0.515>
* m1
#<matrix/double-float (4x4):
   0.238     0.171     0.012     0.094
   0.997     0.448     0.398     0.277
   0.919     0.503     0.782     0.299
   0.681     0.510     0.919     0.515>
* (transpose m1)
#<matrix/double-float (4x4):
   0.238     0.997     0.919     0.681
   0.171     0.448     0.503     0.510
   0.012     0.398     0.782     0.919
   0.094     0.277     0.299     0.515>
* (svd m1)  ;; singular value decomposition
#<matrix/double-float (4x4):
  -0.118    -0.281     0.600    -0.740
  -0.519    -0.694     0.157     0.474
  -0.603    -0.001    -0.664    -0.442
  -0.594     0.663     0.418     0.182>
#<matrix/double-float (4x4):
   2.216     0.000     0.000     0.000
   0.000     0.467     0.000     0.000
   0.000     0.000     0.146     0.000
   0.000     0.000     0.000     0.051>
#<matrix/double-float (4x4):
  -0.679    -0.387    -0.553    -0.289
  -0.659    -0.044     0.704     0.262
  -0.182     0.357    -0.446     0.801
   0.268    -0.849     0.028     0.455>
* (eig m1)  ;; eigenvalues and eigenvectors
(1.678406449809467d0 -0.07999027718616072d0 0.1920127249153754d0
 0.1920127249153754d0)
#<matrix/double-float (4x4):
  -0.100     0.372    -0.135    -0.140
  -0.423    -0.871    -0.326    -0.097
  -0.572    -0.000    -0.026     0.294
  -0.696     0.322     0.871     0.000>
* (qr m1)
#<matrix/double-float (4x4):
   0.155     0.229    -0.903    -0.330
   0.649    -0.576    -0.201     0.454
   0.599    -0.015     0.360    -0.715
   0.443     0.784     0.123     0.416>
#<matrix/double-float (4x4):
   1.536     0.844     1.135     0.602
   0.000     0.174     0.482     0.261
   0.000     0.000     0.304     0.030
   0.000     0.000     0.000     0.096>
* 
```

You will see more lines of output the first time you load **magicl** (ignore any possible compiler warnings).

## Multi-Arm Bandit

TBD

## Markov Decision Processes

TBD

## Overview or Reinforcement Learning: Policies and Value Functions

TBD

## Bellman Equations

TBD

## Dynamic Programming

TBD


