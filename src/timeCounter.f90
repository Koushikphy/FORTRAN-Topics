module timeCounter
    type timer
        integer :: tstart, tcount, trate
        logical :: init = .false.
        contains
        procedure :: resetTimer, getTimer,initTimer,getTimerString
    end type

    contains
    subroutine initTimer(this)  ! start the timer 
        class(timer) :: this
        call system_clock(count_rate=this%trate)
        call system_clock(this%tstart)
        this%init = .true.
    end

    subroutine resetTimer(this)  ! reset the timer to 0
        class(timer) :: this
        call system_clock(this%tstart)
    end

    function getTimer(this)  ! elapsed time as seconds in integer
        class(timer) :: this
        real :: getTimer
        if(.not. this%init) stop "Timer not initialised"
        call system_clock(this%tcount)
        getTimer = real(this%tcount - this%tstart)/real(this%trate)
    end

    function getTimerString(this) ! elapsed time in hour, minute and seconds string format
        class(timer) :: this
        real :: val
        integer :: count, mint, hour, sec
        character(len=13) :: getTimerString
        if(.not. this%init) stop "Timer not initialised"
        call system_clock(count)
        val = real(count - this%tstart)/real(this%trate)
        hour = val/3600   ; val = val - hour*3600
        mint = val/60     ; val = val - mint*60
        sec  = val        ; val = val - sec
        write(getTimerString, "(i0.3,':',i0.2,':',i0.2,f0.3)")hour,mint,sec,val
    end

    function getDateTime() result(dtm) ! returns current date and time
        character(len=20) :: dtm
        integer:: v(8)
        call date_and_time(VALUES=v)
        write(dtm,'(i0.2,"-",i0.2,"-",i4,2x,i0.2,":",i0.2,":",i0.2)') v(3),v(2),v(1),v(5),v(6),v(7)
    end function getDateTime
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
        do j = 1,50000
            k = k+0+2**2+3**3+sqrt(77.0)
        enddo
    enddo

    print *,"1st timer time==>", a%getTimer()
    print *,"2nd timer time==>", b%getTimer()

    print*, a%getTimerString()
end program name