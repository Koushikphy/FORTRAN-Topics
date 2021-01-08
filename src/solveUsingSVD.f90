subroutine solveUsingSVD(A,b,x,m,n)
    ! solves the system Ax=b using SVD (over or under- estimated)
    integer, intent(in)  :: m,n 
    real(kind=8) , intent(in) :: A(m,n), b(m)
    real(kind=8), intent(out) :: x(n)
    real(kind=8) :: ss(n), rcond = -1.0d0
    integer :: irank, info, lwork, nm, nrhs = 1 !<- columns of `b`
    !^ a single `dgelss` call can handle multiple system of equations, 
    !^ to handle such cases use corresponding values of nrhs.
    real(kind=8),allocatable :: work(:)

    nm = max(m, n)
    allocate(work(1))
    lwork = -1
    call dgelss(m, n, nrhs, A, nm, b, nm, ss , rcond, irank, work, lwork, info)


    lwork = work(1) !<- optimul workspace
    deallocate(work); allocate(work(lwork))
    call dgelss(m, n, nrhs, A, nm, b, nm, ss , rcond, irank, work, lwork, info)
    x = b(:n)

end subroutine



program name
    integer, parameter :: n = 4, m =6
    real(kind=8) :: a(m,n) = reshape([ 1.44,-7.84,-4.39, 4.53 & 
                                        ,-9.96,-0.28,-3.24, 3.83 & 
                                        ,-7.55, 3.24, 6.27,-6.64 & 
                                        , 8.34, 8.09, 5.28, 2.06 & 
                                        , 7.08, 2.52, 0.74,-2.47 & 
                                        ,-5.45,-5.70,-1.19, 4.70], [m,n], order=[2,1])
    real(kind = 8) :: b(m) = [8.58,8.26,8.48,-5.28,5.72,8.93], x(n)


        ! Ax=b
        ! -------------
        ! A = 
        !     1.44  -7.84  -4.39   4.53
        !     -9.96  -0.28  -3.24   3.83
        !     -7.55   3.24   6.27  -6.64
        !     8.34   8.09   5.28   2.06
        !     7.08   2.52   0.74  -2.47
        !     -5.45  -5.70  -1.19   4.70
        ! b = 
        !     8.58
        !     8.26
        !     8.48
        !     -5.28
        !     5.72
        !     8.93

    call solveUsingSVD(a,b,x,m,n)
    write(*,'(*(f10.5,5x))')x  !<--- best fit parameters
end program name