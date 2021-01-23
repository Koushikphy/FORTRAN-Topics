program name
    implicit none
    integer, parameter:: nBins = 100
    integer, parameter:: len= 25
    integer, parameter:: points = 1000000
    real(kind=8), parameter :: minVal = 0.0d0, maxVal=1.0d0
    integer :: binCount(nBins), bin ,i,ii
    real(kind=8) :: x, binSize, y

    binSize = (maxVal-minVal)/nBins
    binCount = 0

    ! Central Limit Theorem -> The sample mean from a distribution follows a normal distribution
    
    !$omp parallel do default(shared) private(x,bin,i) 
    do i=1,points
        ! a random variable i.e. sum of `len` random numbers
        x = 0.0d0
        do ii =1,len
            call random_number(y)
            x = x + y
        enddo
        x = x/real(len,8)
        bin = int(x/binSize)+1
        binCount(bin) = binCount(bin) +1
    enddo
    !$omp end parallel do


    do i =1,nBins
        print *, (i-0.5)*binSize, binCount(i)
    enddo


end program name