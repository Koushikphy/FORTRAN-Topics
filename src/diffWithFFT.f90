program name
    implicit none
    integer, parameter :: FFTW_FORWARD = -1
    integer, parameter :: FFTW_EXHAUSTIVE = 8
    integer, parameter :: FFTW_MEASURE = 0
    integer, parameter :: FFTW_BACKWARD = +1
    integer, parameter :: n = 256  ! length of the Fourier space
    real, parameter     :: l = 10   ! length of the acutal space
    real, parameter    :: pi = 4.0*atan(1.0)
    real(kind=8)    :: grid(n),fVal(n),dFVal(n)
    complex(kind=8) :: fK_val(n), dfK_val(n), df_FFT(n)
    integer(kind=8) :: fPlan, bPlan! plans require integer of kind 8
    integer :: i

    ! Create plans for FFT
    call dfftw_plan_dft_1d(fPlan,n,fK_val,fK_val,FFTW_FORWARD,FFTW_MEASURE)
    call dfftw_plan_dft_1d(bPlan,n,fK_val,fK_val,FFTW_BACKWARD,FFTW_MEASURE)


    grid = [( -l/2.0d0+(i-1)*l/real(n-1,8),  i=1,n)]  ! grid=> linspace(-l/2,l/2,n)
    fVal = myFunc(grid)                               ! actual function values
    dFVal= myFuncDiff(grid)                           ! actual derivative values

    ! Forward FFT: convert to Fourier space
    fK_val = fVal  !<- auto-casting real to complex
    call dfftw_execute_dft(fPlan, fK_val, fK_val)
    fK_val = fK_val/sqrt(real(n,kind=8))  !<-- fft doesn't use the constant
    
    
    
    ! derivative of the function in Fourier space: multiply by i*k
    ! NOTE: Careful about the ordering of frequencies, "standard" order is y[1:n/2] positive 
    ! and rest is negetive as "increasing" and y[0] being the zero frequency component.
    dfK_val = fK_val*fftFreq()*cmplx(0,1,kind=8)


    ! convart back to the variable/non-Fourier space
    df_FFT = dfK_val
    call dfftw_execute_dft(bPlan, df_FFT, df_FFT)
    df_FFT = df_FFT/sqrt(real(n,kind=8))  !<-- fft doesn't use the constant



    do i=1,n
        write(111,'(4f15.9)') grid(i), fVal(i), dFVal(i), real(df_FFT(i))
    enddo  !#<--- 3rd and 4th coulumn values should match.



    contains
    elemental function myFunc(x) result(y)  ! a dummy fuction for test
        real(kind=8), intent(in):: x
        real(kind=8) :: y 
        y = cos(x)*exp(-x**2/20)
    end function

    elemental function myFuncDiff(x) result(y) ! derivative of the above dummy function
        real(kind=8), intent(in):: x
        real(kind=8) :: y 
        y = -sin(x)*exp(- x**2/20) -x*myFunc(x)/10.0
    end function

    function fftFreq() result(freq)
        integer      :: nhlen,j
        real(kind=8) :: freq(n)
        nhlen = n/2+1
        freq = (2.0d0*pi/l)*[[(j-1, j=1,nhlen)], [(j-1-n, j=nhlen+1,n)]]  !<-- "standard" FFT frequency order
    end function

end program name