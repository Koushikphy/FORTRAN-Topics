module spline
    implicit none
    integer, parameter:: dp=kind(0.d0) !double precision
    integer(kind=dp) :: nn 
    real(kind=dp),allocatable,dimension(:)    :: xarr, yarr, diff
    private :: xarr, yarr, diff, nn 
    contains


    subroutine splrep(x,y)
        ! takes the s,y input and computes the spline representation
        ! x : independent data points
        ! y : data values corresponding to the x values
        real(kind=dp), intent(in)    :: x(:),y(:)
        integer(kind=dp)             :: i ,k
        real(kind=dp)                :: u(size(x)),sig, p, qn, un, yp1, ypn
        yp1 = 1.0d30
        ypn = 1.0d30

        xarr = x 
        yarr = y 
        nn = size(x)

        allocate(diff(nn))
        if (yp1.gt..99e30) then
            diff(1)=0.
            u(1)=0.
        else
            diff(1)=-0.5
            u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        endif
        do i=2,nn-1
            sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
            p=sig*diff(i-1)+2.
            diff(i)=(sig-1.)/p
            u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        enddo
        if (ypn.gt..99e30) then
            qn=0.0_dp
            un=0.0_dp
        else
            qn=0.5
            un=(3./(x(nn)-x(nn-1)))*(ypn-(y(nn)-y(nn-1))/(x(nn)-x(nn-1)))
        endif
        diff(nn)=(un-qn*u(nn-1))/(qn*diff(nn-1)+1.)
        do k=nn-1,1,-1
            diff(k)=diff(k)*diff(k+1)+u(k)
        enddo
    end subroutine splrep


    elemental real(kind=dp) function splev(xinp) result(yout)
        ! evaluate value at one/multiple input values once the spline representaion is done
        real(kind=dp), intent(in)::xinp
        integer(kind=dp)             :: k, khi, klo
        real(kind=dp)                :: h,a,b

        klo=1
        khi=nn
        do while ((khi-klo).gt.1)
            k=(khi+klo)/2
            if(xarr(k).gt.xinp)then
                khi=k
            else
                klo=k
            endif
        enddo
        h=xarr(khi)-xarr(klo)
        ! if (h.eq.0.) stop "bad x input in splint"  !<<-- elemental functions doesnot allow stop 
        a=(xarr(khi)-xinp)/h
        b=(xinp-xarr(klo))/h
        yout=a*yarr(klo)+b*yarr(khi)+ ((a**3-a)*diff(klo)+(b**3-b)*diff(khi))*(h**2)/6.
    end function splev
end module spline


program test
    use spline
    real(kind=dp) :: a(5)
    a=(/1,4,9,16,25/)
    ! feed the arrays through this call, or one can use public arrays too
    call splrep(a,a**2)
    write(*,*) splev(3.5_dp)
    write(*,*) splev(a)     ! elemental functions equally works with arrays as well as single number
end program test