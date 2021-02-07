! Creats all the required factorials in one shot
! Dynamically calculates new factorial if required
! Useful when repeated factoral calls is required



module factorial
    implicit none
    real(kind=8),allocatable,private :: facts(:)
    integer, private:: factlen
    contains

    !call this to create the precalculated array of factorials
    subroutine initialize_facts(n)
        integer, intent(in) :: n 
        if(allocated(facts)) then               ! factorial calculated once
            if(n>factlen) call increasefacts(n) ! but new factorail values are required
        else                                    ! factorials are not calculated
            call createfacts(n)
        endif
        factlen = n
    end subroutine initialize_facts

    ! at the begineing this creates the array of factorials for a given n
    subroutine createfacts(n)
        integer :: i, n
        allocate(facts(0:n))
        facts(0) = 1.0d0
        do i=1,n
            facts(i) = i*facts(i-1)
        enddo
    end subroutine createfacts

    ! this subroutine dynamically increases the old array of factorails to accomodate for new values
    ! after this length of the array `facts` is changed
    subroutine increasefacts(n)
        integer :: n, i
        real(kind=8),allocatable :: newfacts(:)
        allocate(newfacts(0:n))
        newfacts(0:factlen) = facts
        do i=factlen+1,n
            newfacts(i) = i*newfacts(i-1)
        enddo
        call move_alloc(newfacts, facts)
    end subroutine increasefacts


    function fact(n) result(y)
        integer, intent(in) :: n
        real(kind=8) :: y
        ! Uncomment this if not sure about the range
        ! if(n>factlen) call increasefacts(n)
        y = facts(merge(0,n,n<0)) 
    end function fact
end module factorial



program name
    use factorial
    implicit none
    call initialize_facts(10) ! or createfacts(10)
    print *, fact(-5)
    print *, fact(5)
    print *, fact(11)
    call initialize_facts(12)
    print *, fact(11)
end program name