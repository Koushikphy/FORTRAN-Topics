! Example of using namelist Fortran
! File extension .nml not mandatory

program nml
    implicit none

    character(len=100), parameter :: conffile = 'config.nml'
    character(len=100):: p
    real :: x(3), a, b
    real(kind=8) :: y
    integer :: u
    complex(kind=8) :: z


    namelist /foo/ p, x, y, z
    namelist /bar/ a, b

    !Set some default values, not mandatory though
    x = [1., -1., 3.]
    y = 2.
    z = 3.
    a = 4.
    b = 8.

    ! read values from config.nml, if present
    open(newunit=u, file=trim(conffile), action='read')
    read(u, nml=foo)
    ! read(u, nml=bar)
    close(u)
    print *, x,p, z
    print *, a,b

    ! writing namelist is same as the reading
end program
