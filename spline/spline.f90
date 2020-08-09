module spline_mod
    contains
    subroutine splie2(x1a,x2a,ya,m,n,y2a)
        integer, intent(in) :: m,n
        real (kind=8), intent(in) :: x1a(m),x2a(n),ya(n,m)
        real (kind=8), intent(out) :: y2a(n,m)
        integer :: j
    
        do j=1,m
           call spline(x2a,ya(:,j),n,y2a(:,j))
        enddo
        write(23,'(f20.15)') y2a
    end subroutine splie2
    
    subroutine splin2(x1a,x2a,ya,y2a,m,n,x1,x2,y)
        integer, intent(in) :: m,n
        real (kind=8), intent(in) :: x1a(m),x2a(n),ya(n,m),y2a(n,m)
        real (kind=8), intent(in) :: x1,x2
        real (kind=8), intent(out) :: y
        real (kind=8) :: ytmp(n),yytmp(m)
        integer :: j
    
        do j=1,m
            call splint(x2a,ya(:,j),y2a(:,j),n,x2,yytmp(j))
        enddo
        call spline(x1a,yytmp,m,ytmp)
        call splint(x1a,yytmp,ytmp,m,x1,y)
    end subroutine splin2
    
    
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
    
    
    subroutine splint(xa,ya,y2a,n,x,y)
         integer, intent(in) :: n
         real (kind=8), intent(in) :: xa(n),ya(n),y2a(n),x
         real (kind=8), intent(out) :: y
         real (kind=8) :: h,a,b
         integer :: klo,khi,k
    
        klo=1; khi=n
        if(x<xa(1) .or. x>xa(n)) then 
            print *, x 
            print *, xa(1), xa(n)
            stop
        endif
        do while((khi-klo)>1)
            k=(khi+klo)/2
            if(xa(k)>x)then
                khi=k
            else
                klo=k
            endif
        enddo
    
        h=xa(khi)-xa(klo)
        if (h==0.) STOP "bad xa input in splint"
        a=(xa(khi)-x)/h
        b=(x-xa(klo))/h
        y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
    end subroutine splint

end module
