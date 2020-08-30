program name
    use spline_mod
    implicit none
    integer, parameter :: nth=31,nph=61
    integer, parameter :: newNth=1024, newNph=1024

    integer :: i,j,k,l,ir,n1,n2
    real(kind=8) :: data(nph,nth), xGrid(nth),yGrid(nph),v1,v2,x, diff(nph,nth), dx,dy


    open(1,file="data.dat",status="old")

    do i=1,nth
        do j=1,nph 
            read(1,*)x,xGrid(i),yGrid(j),data(j,i),x,x,x
        enddo
        read(1,*)
    enddo

    call system_clock(count_rate=ir)
    call system_clock(n1)


    call splie2(xGrid,yGrid,data,nth,nph,diff)

    dx = (xGrid(nth)-xGrid(1))/real((newNth-1),8)
    dy = (yGrid(nph)-yGrid(1))/real((newNph-1),8)

    do i = 1,newNth
        v1 = dx*(i-1) + xGrid(1)
        do j=1,newNph
            v2 = dy*(j-1)+ yGrid(1)
            call splin2(xGrid,yGrid,data,diff,nth,nph,v1,v2,x)

            write(21,*)v1,v2,x
        enddo
        write(21,*)
    enddo
    call system_clock(n2)

    print *, '====>', (n2-n1)/real(ir)

end program name