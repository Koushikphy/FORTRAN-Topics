Call c function from Fortran

#### Compile:
```
# compile the c code
gcc -c test.c
# compile the fortran code and link it with the c function
gfortran test.o test.f90 
./a.out
```