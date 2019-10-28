module name1
    implicit none
    real,parameter::y=90.0
end module name1

module name
    use name1
    implicit none
    real :: x
contains
    subroutine init()
        x=5.0
    end subroutine init
end module name

subroutine ing()
    use name 
    print *,x
end subroutine ing

program prog
    use name
    implicit none
    call ing()  ! 0
    call init()
    call ing()  ! 5
end program prog