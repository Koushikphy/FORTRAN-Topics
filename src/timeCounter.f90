module timeCounter
    type timer
        integer :: tstart, tcount, trate
        logical :: init = .false.
        contains
        procedure :: resetTimer, getTimer,initTimer
    end type

    contains
    subroutine initTimer(this)
        class(timer) :: this
        call system_clock(count_rate=this%trate)
        call system_clock(this%tstart)
        this%init = .true.
    end

    subroutine resetTimer(this)
        class(timer) :: this
        call system_clock(this%tstart)
    end

    function getTimer(this)
        class(timer) :: this
        real :: getTimer
        if(.not. this%init) stop "Timer not initialised"
        call system_clock(this%tcount)
        getTimer = real(this%tcount - this%tstart)/real(this%trate)
    end
end


program name
    ! conveniently use multiple time counter that can tracks time in a seperate variable 
    use timeCounter
    implicit none
    integer :: i,j,k
    type(timer) :: a, b 

    ! initiate two seperate counter
    call a%initTimer
    call b%initTimer

    do i=1,10000
        do j = 1,10000
            k = k+0  ! run without optimization
        enddo
    enddo
    print *,"1st timer time==>", a%getTimer()

    call a%resetTimer ! reset counter `a` to 0
    do i=1,10000
        do j = 1,10000
            k = k+0
        enddo
    enddo

    print *,"1st timer time==>", a%getTimer()
    print *,"2nd timer time==>", b%getTimer()

end program name