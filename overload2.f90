! Uage of interface in FORTRAN : Operator overloading
module point
   type point_t
     real :: x
     real :: y
     real :: z
   end type point_t

   ! step 2: extends binary "+"
   interface operator (+)
     module procedure add_offset_to_point
   end interface operator (+)

 contains

   ! step 1: provides interpretation of defined operator
   function add_offset_to_point(op1, op2) result(op3)
     type(point_t), intent(in) :: op1
     real, intent(in)          :: op2
     type(point_t)             :: op3

     op3%x = op1%x + op2
     op3%y = op1%y + op2
     op3%z = op1%z + op2

     print *, "adding real to point"
     print*,op3

   end function add_offset_to_point

 end module point

 program simulator
   use point

   type(point_t) :: pt1, pt2
   real          :: offset

   pt2%x = 10
   pt2%y = 20
   pt2%z = 30
   offset = 100.0

   pt1 = pt2 + offset  ! step 3: using defined operator
   print *, pt1%x, pt1%y, pt1%z
 end program simulator

