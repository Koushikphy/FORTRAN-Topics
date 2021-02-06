!  Run as :  `gfortran spline.f90 main.f90 && ./a.out`
program name
    use splineUtil
    implicit none
    integer, parameter :: nth=31,nph=61, newNth=512, newNph=512
    integer :: i,j
    real(kind=8) :: data(nph,nth), xGrid(nth),yGrid(nph),v1,v2,x, diff(nph,nth), dx,dy
    type(spline2d) :: spl2
    type(spline1d) :: spl1


    open(111,file="data.dat",status="old")
    do i=1,nth
        do j=1,nph 
            read(111,'(3f15.8)')xGrid(i),yGrid(j),data(j,i)
        enddo
        read(111,*)
    enddo
    close(111)


    ! 2D interpolation: (nth x nph) -> (newNth x newNph)
    call spl2%splrep(xGrid, yGrid, data)

    dx = (xGrid(nth)-xGrid(1))/real((newNth-1),8)
    dy = (yGrid(nph)-yGrid(1))/real((newNph-1),8)

    do i = 1,newNth
        v1 = dx*(i-1) + xGrid(1)
        do j=1,newNph
            v2 = dy*(j-1)+ yGrid(1)
            x = spl2%splev(v1,v2)
            write(211,'(3f15.8)')v1,v2,x
        enddo
        write(211,*)
    enddo



    !1D interpolation :  (nph) -> (newNph)
    do j=1,nph ! input for 1D interpolation
        write(112,'(2f15.8)')yGrid(j),data(j,nth)
    enddo

    call spl1%splrep(yGrid, data(:,nth))
    do j=1,newNph
        v1 = dy*(j-1)+ yGrid(1)
        x = spl1%splev(v1)
        write(212,'(2f15.8)')v1,x
    enddo
end program name