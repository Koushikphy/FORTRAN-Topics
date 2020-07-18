program name
    integer, external ::  factorial3
    integer(kind=8), external :: factorial1
    real(kind=8),external :: factorial2, fact
    print *, factorial1(17)
    print *, factorial2(17)
    print *, fact(24)
    print *, factorial3(-5)
end program name


function factorial1(x) result(y)
    implicit none
    integer :: x,i
    integer(kind=8)::y
    y = product((/(i,i=1,x)/))
end function factorial1


function fact(x) result(y)
    implicit none
    integer:: x,i
    real(kind=8) :: y
    y = product((/(i,i=1,x)/))
end function fact


function factorial2(x) result(y)
    implicit none
    integer :: x,i
    real(kind=8)::y
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


double precision recursive function fact(n) result(val)
    integer :: n
    val = merge(1.0d0,n*fact(n-1),n==0)
end function
