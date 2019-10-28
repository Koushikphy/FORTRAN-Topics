module rangefuncs
    contains
    function linspace_r(start, end, bins)
        real(kind=8), intent(in) :: start, end
        integer(kind=8), intent(in):: bins
        real(kind=8):: linspace_r(bins)
        real(kind=8):: step 
        integer(kind=8):: i
        step = (end-start+1)/real(bins,kind=8)
        do i =1,bins
            linspace_r(i) = start + (i-1)*step
        enddo
    end function linspace_r


    function arange(start, end, step)
        real(kind=8), intent(in) :: start, end, step
        real(kind=8), allocatable:: arange(:)
        integer(kind=8):: bins
        integer(kind=8):: i
        bins = nint((end-start)/step)
        allocate(arange(bins))
        do i =1,bins
            arange(i) = start + (i-1)*step
        enddo
    end function arange
end module rangefuncs



    program name
        use rangefuncs
        implicit none
        integer(kind=8) :: x 
        x = 10
        ! print *,linspace_r(1.0d0, 10.0d0, x)
        print *, arange(1.0d0, 10.0d0, .40d0)
    end program name