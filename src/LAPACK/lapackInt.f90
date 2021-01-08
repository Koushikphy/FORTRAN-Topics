module lapack_interface
    contains
    subroutine diagSymTriag(diag, nondiag, z)
        ! eigenvalue of real symmetric tridiagonal matirx, analogous to TQLI of EISPACK
        ! PARAMETERS:
            ! diag   : diagonal elements of the matrix, n values double precision
            !          holds eigenvalues on output
            ! nondiag :off diagonal elements of the matrix, (n-1) values, double precision
            !          destroyed on output
            !z        :OPTIONAL, 2D, (n,n)
            !         if present, z(:,i) holds normalized eigenvector for diag(i)
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
        if(info/=0) stop 'Error in lapack call'
    end subroutine diagSymTriag


    subroutine diagRealSym(a,w)
        ! eigenvalue of real symmetric matirx, analogous to RS of EISPACK
        ! PARAMETERS:
            ! a : 2D, (n,n), input matrix, destroyed
            ! on output a(:,i) is eigenvector of w(i)
            ! w : 1D, (n), output eigenvalues
        real(kind=8),intent(inout) :: a(:,:)
        real(kind=8),intent(out)   :: w(:)
        integer :: info, n, lwork
        integer, parameter :: lwmax = 1000
        real(kind=8) :: work(lwmax)

        n = size(a, dim=1)

        ! optimul workspace query
        lwork = -1
        call dsyev( 'v', 'u', n, a, n, w, work, lwork, info )
        lwork = min( lwmax, int( work( 1 ) ) )
        call dsyev( 'v', 'u', n, a, n, w, work, lwork, info )
        ! call dsyev( 'n', 'u', n, a, n, w, work, lwork, info )
        if(info/=0) stop 'Error in lapack call'
    end subroutine diagRealSym



    subroutine diagRealGen(a,eigVals, eigVecs)
        !`a` is a real8 square matrix
        ! On output `eigVecs(:,i)` stores the (right) eigen vector corresponding to 
        ! the eigen value of `eigVals(i)`. Both are complex as default.

        real(kind=8), intent(in) :: a(:,:)
        complex(kind=8), intent(out) :: eigvals(size(a,1)), eigvecs(size(a,1),size(a,2))
        real(kind=8) , dimension(size(a,1),size(a,2)) :: vl,vr
        real(kind=8) , dimension(size(a,1)) :: wr,wi
        integer :: i,j,n,  lda, ldvl, ldvr, info , lwork
        integer,parameter :: lwmax = 1000
        real(kind=8) :: work(lwmax)
        
        n = size(a,1)
        lda = n ; ldvl = n ; ldvr = n 
    
        ! optimul workspace query
        lwork = -1
        call dgeev( 'v', 'v', n, a, lda, wr, wi, vl, ldvl,vr, ldvr, work, lwork, info )
        lwork = min( lwmax, int( work( 1 ) ) )

        call dgeev( 'v', 'v', n, a, lda, wr, wi, vl, ldvl,vr, ldvr, work, lwork, info )
        eigVals = cmplx(wr,wi,8)
    
        ! returninng only right eigen vector, if left eigenvector is needed then use `vl`
        i = 1
        do while(i<=n)
            if(wi(i)==0.0) then
                eigVecs(:,i) = cmplx(vr(:,i), 0.d0, 8)
                i=i+1
            else 
                eigVecs(:,i) = cmplx(vr(:,i), vr(:,i+1), 8)
                eigVecs(:,i+1) = cmplx(vr(:,i), -vr(:,i+1), 8)
                i=i+2
            endif
        enddo
    
    end subroutine diagRealGen



end module lapack_interface