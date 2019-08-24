! Uage of interface in FORTRAN : Operator overloading
!  @(#) example of overloading operators in fortran
!
!  the following module  allows for using the syntax
!     l1 == l2
!  where l1 and l2 are logical 
!  as an alternative to 
!     l1 .eqv. l2
!  (or the function call
!     boolean_equal (l1, l2)) 
!  or
!     l1 .boolean_equal. l2
!  for that matter); and likewise allows
!     l1 /= l2
!  as equivalent to
!     l1 .neqv. l2
! 
! this is a simplistic example. two significant real-world examples of
! operator overloading to look for use operator overloading to provide
! modules that allow many existing fortran routines to be used with almost
! no source file changes to produce versions using arbitrary precision
! arithmetic, or to provide cumulative error bounds on floating-point
! calculations.
!
! those features would make interesting additions to the fortran standard.
!
! the other purpose of this example is to point out that 
!    l1 == l2   !! should be l1 .eqv. l2
! and 
!    l1 /= l2   !! should be l1 .neqv. l2
! should not work by default; but often do (probably because the compiler
! silently converts logical to integer when a logical appears where a
! numeric value is required?). if you can comment out the use statement
! below and still run this example, your compiler supports this 
! non-standard (but intuitive-looking) syntax. using it makes your code
! much less portable. 
!
!-----------------------------------------------------
module logical_ops
!-----------------------------------------------------
   interface operator ( == )
      module procedure boolean_equal
   end interface operator ( == )
!-----------------------------------------------------
   interface operator ( /= )
      module procedure boolean_notequal
   end interface operator ( /= )
!-----------------------------------------------------
contains
!-----------------------------------------------------
logical function boolean_equal(logical_val1,logical_val2)
   implicit none
   logical, intent (in) :: logical_val1 
   logical, intent (in) :: logical_val2 

   if (logical_val1 .eqv. logical_val2 )then
     boolean_equal=.true.
   else
     boolean_equal=.false.
   endif

end function boolean_equal
!-----------------------------------------------------
logical function boolean_notequal(logical_val1,logical_val2)
   implicit none
   logical, intent (in) :: logical_val1 
   logical, intent (in) :: logical_val2 

   if (logical_val1 .eqv. logical_val2 )then
     boolean_notequal=.false.
   else
     boolean_notequal=.true.
   endif

end function boolean_notequal
!-----------------------------------------------------
end module logical_ops
!-----------------------------------------------------

program testit

   use logical_ops

   if (.true. .eqv. .true. )then      ! standard
      write(*,*)'hello world .eqv.'
   endif

   if (.true. .neqv. .false. )then    ! standard
      write(*,*)'hello world .neqv.'
   endif

   if (.true. == .true. )then         ! using overloaded operator 
      write(*,*)'hello world =='
   endif

   if (.true. /= .false. )then        ! using overloaded operator 
      write(*,*)'hello world /='
   endif

end program testit