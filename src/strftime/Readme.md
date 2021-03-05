Return current time stamp in a given format. Analogous to standard strftime function available in C/C++/Python. In fact it uses C `strftime` under the hood.  
strftime reference : https://www.cplusplus.com/reference/ctime/strftime/  

#### Compile:
```
# compile the c code
gcc -c time.c
# compile the fortran code and link it with the c function
gfortran time.o time.f90 
./a.out
```