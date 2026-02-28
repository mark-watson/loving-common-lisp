# Linear Algebra Using the MAGICL Library

## Installation

MAGICL uses either the BLAS or LAPACK packages for efficient array calculations and the many linear algebra functions provided in these packages. BLAS and LAPACK are partially written in FORTRAN and may not be easy to install on your system. Fear not! You can run MAGICL (somewhat slowly) using their pure Common Lisp backend that can be installed:

- cd ~/~/quicklisp/local-projects
- git clone https://github.com/quil-lang/magicl.git

Then you can simply use the pure Common Lisp backend; for example:

```lisp
(magicl.backends:with-backends (:lisp)
  (print "OK"))
```

That said, try to install one of the much faster backends using the [documentation for installing dependencies](https://github.com/quil-lang/magicl/blob/master/doc/requirements.md).

