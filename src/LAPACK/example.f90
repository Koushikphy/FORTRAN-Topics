program name
    use lapack_interface
    implicit none
    integer, parameter :: nn = 5 
    real(kind=8) :: aa(nn,nn), bbr(nn), ccr(nn,nn), a11(nn), a22(nn)
    integer :: i,j
    
    complex(kind=8)::bbc(nn), ccc(nn,nn)


    ! example : Real , geneeral matrix
    aa = reshape([-1.01, 3.98, 3.30, 4.43, 7.31, &
            0.86, 0.53, 8.26, 4.96,-6.43, &
            -4.60,-7.04,-3.89,-7.66,-6.16, &
            3.31, 5.29, 8.20,-7.33, 2.47, &
            -4.81, 3.55,-1.51, 6.18, 5.58],[5,5])

    call diagRealGen(aa,bbc,ccc)

    write(*,'(*(2f8.3,5x))') bbc
    write(*,*)repeat('-',120)
    do i =1,nn
        write(*,'(*(2f8.3,5x))') ccc(i,:)
    enddo
    write(*,'(////)')

    !example real symmetric
    aa = reshape([ 1.96,-6.49,-0.47,-7.20,-0.65, &
        -6.49, 3.80,-6.39, 1.50,-6.34, &
        -0.47,-6.39, 4.17,-1.51, 2.67, &
        -7.20, 1.50,-1.51, 5.70, 1.80, &
        -0.65,-6.34, 2.67, 1.80,-7.10],[5,5])

    call diagRealSym(aa,bbr)

    write(*,'(*(f8.3,5x))') bbr
    write(*,*)repeat('-',120)
    do i =1,nn
        write(*,'(*(f8.3,5x))') aa(i,:)
    enddo
    write(*,'(////)')

    ! aa = reshape([ 2,-1, 0, 0, 0, &
    !               -1, 2,-1, 0, 0, &
    !                0,-1, 2,-1, 0, &
    !                0, 0,-1, 2,-1, &
    !                0, 0, 0,-1, 2],[5,5])
    a11 = 2 
    a22 = -1
    call diagSymTriag(a11,a22,ccr)
    write(*,'(*(f8.3,5x))') a11
    write(*,*)repeat('-',120)
    do i =1,nn
        write(*,'(*(f8.3,5x))') ccr(i,:)
    enddo
    write(*,'(////)')

end program name