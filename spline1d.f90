module spline
    implicit none
    integer(8) :: nn 
    real(8),allocatable,dimension(:)    :: xarr, yarr, diff
    private :: xarr, yarr, diff, nn 
    contains


    subroutine splrep(x,y,n)
        integer(8),  intent(in):: n
        real(8), intent(in)    :: x(n),y(n)
        integer(8)             :: i ,k
        real(8)                :: u(n),sig, p, qn, un, yp1, ypn
        yp1 = 1.0d30
        ypn = 1.0d30
        allocate(diff(n))
        xarr = x 
        yarr = y 
        nn=n
        if (yp1.gt..99e30) then
            diff(1)=0.
            u(1)=0.
        else
            diff(1)=-0.5
            u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        endif
        do i=2,n-1
            sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
            p=sig*diff(i-1)+2.
            diff(i)=(sig-1.)/p
            u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        enddo
        if (ypn.gt..99e30) then
            qn=0.
            un=0.
        else
            qn=0.5
            un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        endif
        diff(n)=(un-qn*u(n-1))/(qn*diff(n-1)+1.)
        do k=n-1,1,-1
            diff(k)=diff(k)*diff(k+1)+u(k)
        enddo
    end subroutine splrep


    elemental real(8) function splev(xinp) result(yout)
        real(8), intent(in)::xinp
        integer(8)             :: k, khi, klo
        real(8)                :: h,a,b

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
    real(8) :: a(5)
    a=(/1,4,9,16,25/)
    ! feed the arrays through this call, or one can use public arrays too
    call splrep(a,a**2,int(5, kind=8))
    write(*,*) splev(3.5d0)
    write(*,*) splev(a)     ! elemental functions equally works with arrays as well as single number
end program test