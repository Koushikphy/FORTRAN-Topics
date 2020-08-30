### Compiling with LAPACK
Provided the LAPACK library is available in the path,
```
gfortran program.f90 -llapack
```
When using Intel Fortran compiler (ifort) `-mkl` option can be used insted to use the Intel Math Kernel library


### Useful links:
1. [LAPACK user guide](https://www.netlib.org/lapack/lug/)
1. [Naming Scheme of LAPACK subroutine](https://www.netlib.org/lapack/lug/node24.html)
2. [Get source codes for individual routine](http://www.netlib.org/lapack/individualroutines.html)
1. [LAPACK-BLAS subroutine catalog 1](http://www.netlib.org/lapack/explore-html/modules.html)
1. [LAPACK-BLAS subroutine catalog 2](http://www.icl.utk.edu/~mgates3/docs/lapack.html)
1. [LAPACK95](http://www.netlib.org/lapack95/) a Fortran95 interface to LAPACK with generic interface, optional arguments etc.