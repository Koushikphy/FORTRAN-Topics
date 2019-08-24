! Uage of interface in FORTRAN : Operator overloading
! function newFunc(x,x1) result(y)
!     real, intent(in) :: x,x1
!     real :: y
!     y=sqrt(x**2+x1**2)
! end function newFunc


! program myProgram
!    implicit none
!    real :: a,b,c
!    interface operator(*)  ! this means call * on two real value to return norm, ---> operator collision
!       real function newFunc(x,x1) result (y)
!          real, intent(in) :: x,x1
!       end function newFunc
!    end interface operator(*) 
!    a=3.0
!    b=4.0
!    c=newFunc(a,b)
!    print *,c
! end program myProgram


! function newFunc(x,x1) result(y)
!     real, intent(in) :: x,x1
!     real :: y
!     y=sqrt(x**2+x1**2)
! end function newFunc


! program myProgram
!    implicit none
!    real :: a,b,c
!    interface operator(.no.)   ! so we define a new operator that does not collide with other and works
!       real function newFunc(x,x1) result (y)
!          real, intent(in) :: x,x1
!       end function newFunc
!    end interface operator(.no.) 
!    a=3.0
!    b=4.0
!    c=a.no.b
!    print *,c
! end program myProgram


! a new type that hides the real type, because modified * can't work on intrinsic real type
module newT
 type myreal
 real x
 end type myreal
end module newT


function newFunc(x,x1) result(y)
   use newT
   type(myreal), intent(in) :: x,x1
   real :: y
   y=sqrt(x%x**2+x1%x**2)   ! takes `x` argument of myreal and calculate norm
end function newFunc


program myProgram
   use newT
  implicit none
  type(myreal) :: a,b
  real c
  interface operator(*) ! the * operator now operates over the myreal type
     real function newFunc(x,x1) result (y)
     use newT
     type(myreal), intent(in) :: x,x1
     end function newFunc
  end interface operator(*) 
  a%x=3.0
  b%x=4.0
  c=a*b! the * operator now operates over the myreal type
  print *,c
end program myProgram