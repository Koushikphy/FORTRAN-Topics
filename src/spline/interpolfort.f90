subroutine locate(xx,n,x,l)
    integer, intent(in) :: n
    real(8),    intent(in) :: xx(n),x
    integer, intent(out):: l
    integer             :: u,k

    l=0; u=n

    do while((u-l)>1)
        k=(u+l)/2
        if((x>xx(k)))then
            l=k
        else
            u=k
        endif
    enddo
end subroutine locate


subroutine newInterPol(tau,xG,yG,nX,nY,nN,x,y,out)
    implicit none
    ! `tau`,,, `nN` values on a grid of `nX`/`nY`
    integer,intent(in) :: nX,nY,nN
    real(kind=8),intent(in) :: tau(nN,nY,nX),xG(nX),yG(nY),x,y
    real(kind=8), intent(out) ::out(nN)
    real(kind=8) :: dx,dy, yf(nN,4),yf1(nN,4),yf2(nN,4),yf12(nN,4), xl,xu,yl,yu, d1,d2
    integer :: ii1,ii2,jj1,jj2,k


    dx=xG(2)-xG(1)
    dy=yG(2)-yG(1)

    call locate(xG,nX,x,ii1)
    ! ii1 = int((x-xG(1))/dx)+1  ! for evenly-spaced grid
    ii2=ii1+1
    xl=xG(ii1)
    xu=xG(ii2)

    call locate(yG,nY,y,jj1)
    ! jj1 = int((y-yG(1))/dy)+1
    jj2=jj1+1
    yl=yG(jj1)
    yu=yG(jj2)


    d1 = xu-xl
    d2 = yu-yl

    !zeroth derivatives
    yf(:,1)=tau(:,jj1,ii1)
    yf(:,2)=tau(:,jj1,ii2)
    yf(:,3)=tau(:,jj2,ii2)
    yf(:,4)=tau(:,jj2,ii1)
    !first derivatives
    ! print *, yf; stop

    if(ii1==1)then
        !forward differencing(f.d)
        yf1(:,1)=(tau(:,jj1,ii1+1)-tau(:,jj1,ii1))/dx
        yf1(:,4)=(tau(:,jj2,ii1+1)-tau(:,jj2,ii1))/dx
    else
        !central differencing(c.d)
        yf1(:,1)=0.5d0*(tau(:,jj1,ii1+1)-tau(:,jj1,ii1-1))/dx
        yf1(:,4)=0.5d0*(tau(:,jj2,ii1+1)-tau(:,jj2,ii1-1))/dx
    endif

    if(ii2==nX)then
        !backward differencing(b.d)
        yf1(:,2)=(tau(:,jj1,ii2)-tau(:,jj1,ii2-1))/dx
        yf1(:,3)=(tau(:,jj2,ii2)-tau(:,jj2,ii2-1))/dx
    else
        !c.d
        yf1(:,2)=0.5d0*(tau(:,jj1,ii2+1)-tau(:,jj1,ii2-1))/dx
        yf1(:,3)=0.5d0*(tau(:,jj2,ii2+1)-tau(:,jj2,ii2-1))/dx
    endif


    if(jj1==1)then
        !f.d
        yf2(:,1)=(tau(:,jj1+1,ii1)-tau(:,jj1,ii1))/dy
        yf2(:,2)=(tau(:,jj1+1,ii2)-tau(:,jj1,ii2))/dy
    else
        !c.d
        yf2(:,1)=0.5d0*(tau(:,jj1+1,ii1)-tau(:,jj1-1,ii1))/dy
        yf2(:,2)=0.5d0*(tau(:,jj1+1,ii2)-tau(:,jj1-1,ii2))/dy
    endif

    if(jj2==nY)then
        !b.d
        yf2(:,3)=(tau(:,jj2,ii2)-tau(:,jj2-1,ii2))/dy
        yf2(:,4)=(tau(:,jj2,ii1)-tau(:,jj2-1,ii1))/dy
    else
        !c.d
        yf2(:,3)=0.5d0*(tau(:,jj2+1,ii2)-tau(:,jj2-1,ii2))/dy
        yf2(:,4)=0.5d0*(tau(:,jj2+1,ii1)-tau(:,jj2-1,ii1))/dy
    endif

    !second derivatives
    if(ii1==1.and.jj1==1)then
        !f.d & f.d
        yf12(:,1)=((tau(:,jj1+1,ii1+1)-tau(:,jj1+1,ii1))-(tau(:,jj1,ii1+1)-tau(:,jj1,ii1)))/dx/dy
    elseif(ii1==1.and.jj1/=1)then
        !f.d & c.d
        yf12(:,1)=0.5d0*((tau(:,jj1+1,ii1+1)-tau(:,jj1+1,ii1))-(tau(:,jj1-1,ii1+1)-tau(:,jj1-1,ii1)))/dx/dy
    elseif(ii1/=1.and.jj1==1)then
        !c.d & f.d
        yf12(:,1)=0.5d0*((tau(:,jj1+1,ii1+1)-tau(:,jj1+1,ii1-1))-(tau(:,jj1,ii1+1)-tau(:,jj1,ii1-1)))/dx/dy
    else
        !c.d & c.d
        yf12(:,1)=0.25d0*((tau(:,jj1+1,ii1+1)-tau(:,jj1+1,ii1-1))-(tau(:,jj1-1,ii1+1)-tau(:,jj1-1,ii1-1)))/dx/dy
    endif
    

    if(ii2==nX.and.jj1==1)then
        !b.d & f.d
        yf12(:,2)=((tau(:,jj1+1,ii2)-tau(:,jj1+1,ii2-1))-(tau(:,jj1,ii2)-tau(:,jj1,ii2-1)))/dx/dy
    elseif(ii2==nX.and.jj1/=1)then
        !b.d & c.d
        yf12(:,2)=0.5d0*((tau(:,jj1+1,ii2)-tau(:,jj1+1,ii2-1))-(tau(:,jj1-1,ii2)-tau(:,jj1-1,ii2-1)))/dx/dy
    elseif(ii2/=nX.and.jj1==1)then
        !c.d & f.d
        yf12(:,2)=0.5d0*((tau(:,jj1+1,ii2+1)-tau(:,jj1+1,ii2-1))-(tau(:,jj1,ii2+1)-tau(:,jj1,ii2-1)))/dx/dy
    else
        !c.d & c.d
        yf12(:,2)=0.25d0*((tau(:,jj1+1,ii2+1)-tau(:,jj1+1,ii2-1))-(tau(:,jj1-1,ii2+1)-tau(:,jj1-1,ii2-1)))/dx/dy
    endif


    if(ii2==nX.and.jj2==nY)then
        !b.d & b.d
        yf12(:,3)=((tau(:,jj2,ii2)+tau(:,jj2,ii2-1))-(tau(:,jj2-1,ii2)-tau(:,jj2-1,ii2-1)))/dx/dy
    elseif(ii2==nX.and.jj2/=nY)then
        !b.d & c.d
        yf12(:,3)=0.5d0*((tau(:,jj2+1,ii2)+tau(:,jj2+1,ii2-1))-(tau(:,jj2-1,ii2)-tau(:,jj2-1,ii2-1)))/dx/dy
    elseif(ii2/=nX.and.jj2==nY)then
        !c.d & b.d
        yf12(:,3)=0.5d0*((tau(:,jj2,ii2+1)+tau(:,jj2,ii2-1))-(tau(:,jj2-1,ii2+1)-tau(:,jj2-1,ii2-1)))/dx/dy
    else
        !c.d & c.d
        yf12(:,3)=0.25d0*((tau(:,jj2+1,ii2+1)+tau(:,jj2+1,ii2-1))-(tau(:,jj2-1,ii2+1)-tau(:,jj2-1,ii2-1)))/dx/dy
    endif

    !f.d & b.d
    if(ii1==1.and.jj2==nY)then
        yf12(:,4)=((tau(:,jj2,ii1+1)-tau(:,jj2,ii1))-(tau(:,jj2-1,ii1+1)-tau(:,jj2-1,ii1)))/dx/dy
    elseif(ii1==1.and.jj2/=nY)then
        !f.d & c.d
        yf12(:,4)=0.5d0*((tau(:,jj2+1,ii1+1)-tau(:,jj2+1,ii1))-(tau(:,jj2-1,ii1+1)-tau(:,jj2-1,ii1)))/dx/dy
    elseif(ii1/=1.and.jj2==nY)then
        !c.d & b.d
        yf12(:,4)=0.5d0*((tau(:,jj2,ii1+1)-tau(:,jj2,ii1-1))-(tau(:,jj2-1,ii1+1)-tau(:,jj2-1,ii1-1)))/dx/dy
        else
        !c.d & c.d
        yf12(:,4)=0.25d0*((tau(:,jj2+1,ii1+1)-tau(:,jj2+1,ii1-1))-(tau(:,jj2-1,ii1+1)-tau(:,jj2-1,ii1-1)))/dx/dy
    endif


    do k=1,nN
        call bcuint(yf(k,:),yf1(k,:),yf2(k,:),yf12(k,:),out(k))
    enddo

    contains

    subroutine bcuint(y0,y1,y2,y12,ansy)  

        ! This subroutine is used as a secondary subroutine in 'interpol'

        real(8), intent(in) :: y0(4),y1(4),y12(4),y2(4)
        real(8), intent(out)::ansy
        real(kind=8) :: c(4,4),xx(16),wt(16,16),t,u
        integer:: i
        DATA wt /1,0,-3,2,4*0,-3,0,9,-6,2,0,-6,4,&
            8*0,3,0,-9,6,-2,0,6,-4,10*0,9,-6,2*0,-6,4,2*0,3,-2,6*0,-9,6,&
            2*0,6,-4,4*0,1,0,-3,2,-2,0,6,-4,1,0,-3,2,8*0,-1,0,3,-2,1,0,-3,&
            2,10*0,-3,2,2*0,3,-2,6*0,3,-2,2*0,-6,4,2*0,3,-2,0,1,-2,1,5*0,&
            -3,6,-3,0,2,-4,2,9*0,3,-6,3,0,-2,4,-2,10*0,-3,3,2*0,2,-2,2*0,&
            -1,1,6*0,3,-3,2*0,-2,2,5*0,1,-2,1,0,-2,4,-2,0,1,-2,1,9*0,-1,2,&
            -1,0,1,-2,1,10*0,1,-1,2*0,-1,1,6*0,-1,1,2*0,2,-2,2*0,-1,1/
        

        xx=[y0, y1*d1, y2*d2, y12*d1*d2]
        c = reshape(matmul(wt,xx),[4,4], order=[2,1])
        !^^^ call bcucof (y,y1,y2,y12,xu-xl,yu-yl,c)

        t=(x-xl)/d1
        u=(y-yl)/d2

        ansy=0.0d0
        do i=4,1,-1
            ansy =t*ansy+((c(i,4)*u+c(i,3))*u+c(i,2))*u+c(i,1)
        enddo

    end subroutine bcuint

end
