program name
    implicit none
    real, external :: testf , testf_alt
    print *, testf()
    print *, testf()
    print *, testf_alt()
    print *, testf_alt()
    call sub1
    call sub1 
    call sub2
    call sub2
end program name

! initializtion during decalaration is done during compilation
! so in runtime successive calls to the function retains the last value

real function testf()
    implicit none
    real :: ke = 0.0   ! <- bad
    ke=ke+1
    testf = .5*ke
end function testf


real function testf_alt()
    implicit none
    real :: ke
    ke=0               ! <- good
    ke=ke+1
    testf_alt = .5*ke
end function testf_alt


! same happens with subroutines
subroutine sub1()
    implicit none
    real :: x = 1
    x = x+1
    print *, x
end subroutine sub1


subroutine sub2()
    implicit none
    real :: x 
    x = 1
    x=x+1
    print *, x
end subroutine sub2