module lapack_interface
    contains
    subroutine diag_sym_triag(diag, nondiag, z)
        ! eigenvalue of real symmetric tridiagonal matirx, analogous to TQLI of EISPACK
        ! PARAMETERS:
            ! diag   : diagonal elements of the matrix, n values double precision
            !          holds eigenvalues on output
            ! nondiag :off diagonal elements of the matrix, (n-1) values, double precision
            !          destroyed on output
            !z        :OPTIONAL, 2D, (n,n)
            !         z(:,i) holds normalized eigenvector for diag(i)
        real(kind=8),intent(inout) :: diag(:), nondiag(:)
        real(kind=8),intent(inout),optional :: z(:,:)
        real(kind=8)::x
        real(kind=8),allocatable :: y(:)
        integer :: info,n
        n = size(diag)
        if(present(z)) then
            allocate(y(max(1,2*n-2)))
            call dstev('v', n, diag, nondiag, z, n, y, info)
        else
            call dstev('N', n, diag, nondiag, x, 1, y, info)
        endif
        if(info/=0) stop 'Error'
    end subroutine diag_sym_triag


    subroutine diag_sym(a,w,ev)
        ! eigenvalue of real symmetric matirx, analogous to RS of EISPACK
        ! PARAMETERS:
            ! a : 2D, (n,n), input matrix, destroyed
            !   if `ev` is true, then a(:,i) is eigenvector of w(i)
            ! w : 1D, (n), output eigenvalues
            ! ev: logical, if true calculates the eigenvectors
        real(kind=8),intent(inout) :: a(:,:)
        real(kind=8),intent(out)   :: w(:)
        logical,intent(in),optional:: ev
        logical :: ev_
        integer :: info, n, lwork
        real(kind=8) :: work(1000)

        n = size(a, dim=1)
        ev_ = .false.
        if(present(ev)) ev_ = ev

        lwork = -1 ! optimul workspace query
        call dsyev( 'v', 'u', n, a, n, w, work, lwork, info )
        lwork = min( 1000, int( work( 1 ) ) )

        if(ev_) then
            call dsyev( 'v', 'u', n, a, n, w, work, lwork, info )
        else
            call dsyev( 'n', 'u', n, a, n, w, work, lwork, info )
        endif
        if(info/=0) stop 'Error'
    end subroutine diag_sym
end module lapack_interface