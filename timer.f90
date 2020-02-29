module timer
    integer,private :: end , rate, n
    integer, private, allocatable :: start(:)
    contains

    subroutine initTimer()
        call system_clock(count_rate=rate)
        n=0
    end

    function startTimer() result(timerIndex)
        integer:: tStart, timerIndex
        integer, allocatable :: newStarts(:)
        call system_clock(tStart)
        n = n+1
        if(n==1) then ! first time
            allocate(start(n))
            start(1) = tStart
        else
            allocate(newStarts(n))
            newStarts(:n-1) = start
            newStarts(n) = tStart
            call move_alloc(newStarts, start)
        endif
        timerIndex = n
    end

    function logTimer(timer) result(time)
        integer :: timer, now
        real(kind=8) :: time
        if(timer>n) then
            write(*,*) 'No timer started with', timer
            return 
        endif
        call system_clock(now)
        time = real(now - start(timer), kind=8)/real(rate, kind=8)
    end

end module timer



program name
    use timer
    implicit none
    integer:: i,j,k
    call initTimer
    i = startTimer()
    j = startTimer()
    k = startTimer()

    print *, logTimer(i)
    print *, logTimer(j)
    print *, logTimer(k)
end program name