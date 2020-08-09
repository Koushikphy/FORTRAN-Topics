module spline_mod_alloc
    integer :: nY,nX
    real(kind=8), allocatable,private :: yGrid(:),xGrid(:),zVals(:,:),diffs(:,:)
    contains

    subroutine initVars(xG,yG,nGX,nGY)  ! inital call to setup variable
        integer, intent(in) :: nGX,nGY
        real(kind=8),intent(in) :: xG(nGX), yG(nGY)
        nY = nGY
        nX = nGX
        allocate(xGrid(nX),yGrid(nY),zVals(nY,nX),diffs(nY,nX))
        yGrid = yG
        xGrid = xG
    end subroutine

    subroutine splie2(zValsIn) ! called one time to calculate all the differentiation to use
        real (kind=8), intent(in) ::zValsIn(nY,nX)
        integer :: j
        zVals = zValsIn
        do j=1,nX
           call spline(yGrid,zVals(:,j),nY,diffs(:,j))
        enddo
        write(22,'(f20.15)') diffs

    end subroutine splie2
    
    function splin2(x,y) result(z) ! calculate `z` from value of `x,y`
        real (kind=8) :: x,y,z,ytmp(nY),yytmp(nX)
        integer :: j
    
        do j=1,nX
            call splint(yGrid,zVals(:,j),diffs(:,j),nY,y,yytmp(j))
        enddo
        call spline(xGrid,yytmp,nX,ytmp)

        ! FIX : if this is called again with same y but different x, the above calculation is done again, unnecessarily
        call splint(xGrid,yytmp,ytmp,nX,x,z)
    end function splin2


    ! If we just want to do interpolation on a rectangular grid then this one is slightly efficient, but the result does
    ! suffer from cache mismatch
    subroutine splin2Grid(n1,n2)
        ! real(kind=8), intent(out):: zOut(n2,n1)
        real (kind=8) :: x,y,z,ytmp(nY),yytmp(nX),dx,dy,v2,v1
        integer :: i,j,k,l

        dx = (xGrid(nX)-xGrid(1))/real((n1-1),8)
        dy = (yGrid(nY)-yGrid(1))/real((n2-1),8)

        do k=1,n2
            v2 = dy*(k-1)+ yGrid(1)
            do j=1,nX
                call splint(yGrid,zVals(:,j),diffs(:,j),nY,v2,yytmp(j))
            enddo
            call spline(xGrid,yytmp,nX,ytmp)

            do i=1,n1
                v1 = dx*(i-1) + xGrid(1)
                call splint(xGrid,yytmp,ytmp,nX,v1,z)
                write(222,*)v1,v2,z
            enddo
            write(222,*)

        enddo
    end 




    subroutine spline(x,y,n,y2)
        integer, intent(in) :: n
        real (kind=8), intent(in) :: x(n),y(n)
        real (kind=8), intent(out) :: y2(n)
        real (kind=8) :: u(n),sig,p,qn,un
        integer :: i,k
    
        y2(1)=0.0_8; u(1)=0.0_8
    
        do i=2,n-1
            sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
            p=sig*y2(i-1)+2.
            y2(i)=(sig-1.)/p
            u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
        enddo
    
        qn=0.0_8; un=0.0_8
    
        y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
        do k=n-1,1,-1
            y2(k)=y2(k)*y2(k+1)+u(k)
        enddo
    end subroutine spline


    subroutine splint(xInp,yInp,yDiff,n,x,y)
         integer, intent(in) :: n
         real (kind=8), intent(in) :: xInp(n),yInp(n),yDiff(n),x
         real (kind=8), intent(out) :: y
         real (kind=8) :: h,a,b
         integer :: klo,khi,k
    
        klo=1; khi=n
    
        do while((khi-klo)>1)
            k=(khi+klo)/2
            if(xInp(k)>x)then
                khi=k
            else
                klo=k
            endif
        enddo
    
        h=xInp(khi)-xInp(klo)
        if (h==0.) STOP "bad xInp input in splint"
        a=(xInp(khi)-x)/h
        b=(x-xInp(klo))/h
        y=a*yInp(klo)+b*yInp(khi)+((a**3-a)*yDiff(klo)+(b**3-b)*yDiff(khi))*(h**2)/6.
    end subroutine splint

end module
