program name
    integer, external :: factorial1, factorial2, factorial3
    print *, factorial1(5)
    print *, factorial2(5)
    print *, factorial3(5)
end program name


function factorial1(x) result(y)
    implicit none
    integer :: x,y,i
    y = product((/(i,i=1,x)/))
end function factorial1


function factorial2(x) result(y)
    implicit none
    integer :: x,y,i
    if (x.eq.0) then
        y=1.0d0
    else
        y=1.0d0
        do i=1,x
           y=y*i
        enddo
    endif
end function factorial2


function factorial3(x) result(y)
    implicit none
    integer :: x,y
    y = int(gamma(real(x+1)))
end function factorial3