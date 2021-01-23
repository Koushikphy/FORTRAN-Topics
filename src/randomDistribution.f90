program name
    implicit none
    integer, parameter:: nBins = 100
    integer, parameter:: points = 1000000
    real(kind=8), parameter :: minVal = 0.0d0, maxVal=1.0d0
    integer :: binCount(nBins), bin ,i
    real(kind=8) :: x, binSize

    binSize = (maxVal-minVal)/nBins
    binCount = 0

    !$omp parallel do default(shared) private(x,bin,i) 
    do i=1,points
        call random_number(x)
        bin = int(x/binSize)+1
        binCount(bin) = binCount(bin) +1
    enddo
    !$omp end parallel do

    do i =1,nBins
        print *, (i-0.5)*binSize, binCount(i)
    enddo


end program name