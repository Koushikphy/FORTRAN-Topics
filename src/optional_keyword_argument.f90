program name
    implicit none
    interface
    subroutine asimplesub(xx, y, z) !<-- notice the actual arguments name changed
        implicit none
        integer,intent(in) :: xx,y
        integer,optional :: z
        end subroutine asimplesub
    end interface
    ! to use optional/keyword arguments internal proc or an interface block is required
    call asimplesub(1,2)
    call asimplesub(1,2,3)
    ! keywords will be same as mentioned in interface block
    call asimplesub(xx=1,y=2,z=3)
    call asimplesub(z=3,xx=1,y=2)
    call asimplesub(y=2,xx=1)
    ! prints----------------------------------
    ! 1           2         999
    ! 1           2           3
    ! 1           2           3
    ! 1           2           3
    ! 1           2         999
    !-----------------------------------------
end program name



!`z_` is a dummy optional
subroutine asimplesub(x, y, z_) 
    implicit none
    integer,intent(in) :: x,y
    integer,optional :: z_
    integer:: z
    z=999
    if(present(z_)) z=z_
    print *,x,y,z
end subroutine asimplesub