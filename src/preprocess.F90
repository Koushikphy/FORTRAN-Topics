program name
    implicit none
    ! compile as gfortran preprocess.F90 -Dvar=9
#define var2 77
#ifdef var
    print *, 'var provided', var
#else
    print *, 'var not provided'
#endif

!--or--
#ifndef var
    print *, 'var not provided'
#else
    print *, 'var provided', var
#endif
print *, 'var2==', var2

end program name