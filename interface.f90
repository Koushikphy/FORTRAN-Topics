! how to use interface in fortran

! function newFunc(x,x1) result(y)
!     real, intent(in) :: x,x1
!     real :: y
!     y=sqrt(x**2+x1**2)
! end function newFunc
! ! THIS works but the main program doesn't have information about the function
! ! one can't tell just by looking at the main program what the function does
! program myProgram
!    implicit none
!    real :: a,b,c,newFunc ! the function is just a variable like others
!    a=3.0
!    b=4.0
!    c=newFunc(a,b)
!    print *,c
! end program myProgram



function newFunc(x,x1) result(y)
    real, intent(in) :: x,x1
    real :: y
    y=sqrt(x**2+x1**2)
end function newFunc

! introduce `interface`, one can directly undrestand what the `newfunc` program does just by lookin the interface block
program myProgram
   implicit none
   real :: a,b,c
   interface
      real function newFunc(x,x1) result (y)
         real, intent(in) :: x,x1
      end function newFunc
   end interface 
   a=3.0
   b=4.0
   c=newFunc(a,b)
   print *,c
end program myProgram