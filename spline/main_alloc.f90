program name
    use spline_mod_alloc
    implicit none
    integer, parameter :: nth=31,nph=61
    integer, parameter :: newNth=1024, newNph=1024
    ! integer, parameter :: newNth=91, newNph=181

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

    call initVars(xGrid,yGrid,nth,nph)

    call splie2(data)

    dx = (xGrid(nth)-xGrid(1))/real((newNth-1),8)
    dy = (yGrid(nph)-yGrid(1))/real((newNph-1),8)

    do i = 1,newNth
        v1 = dx*(i-1) + xGrid(1)
        do j=1,newNph
            v2 = dy*(j-1)+ yGrid(1)

            write(221,*)v1,v2,splin2(v1,v2)
        enddo
        write(221,*)
    enddo
    call system_clock(n2)

    print *, '====>', (n2-n1)/real(ir)

    call splin2Grid(newNth, newNph)


    call system_clock(n1)

    print *, '====>', (n1-n2)/real(ir)


end program name