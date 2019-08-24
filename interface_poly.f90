! usage of interface : procedure polymorphism
! A program which uses this module now has access to a single logical function f which accepts 
! arguments which are of the integer, real, or complex data types. The return type of the function is 
! the same as the input type. In this way the routine is much like many of the intrinsic functions defined 
! as part of the Fortran standard. An example program is given below:
module extension_mod
   implicit none
   private

   interface f
      module procedure f_i
      module procedure f_r
      module procedure f_z
   end interface

   public::f

contains

   function f_i(x) result(y)
      integer,intent(in)::x
      integer::y

      y = x**2-1
   end function f_i

   function f_r(x) result(y)
      real,intent(in)::x
      real::y

      y = x**2-1.0
   end function f_r

   function f_z(x) result(y)
      complex,intent(in)::x
      complex::y

      y = x**2-1.0
   end function f_z

end module extension_mod


program main
   use extension_mod
   implicit none

   integer::xi,yi
   real::xr,yr
   complex::xz,yz

   xi = 2
   xr = 2.0
   xz = complex(2.0,0)

   ! only one function `f` handles input-output for different types simultaneously
   ! `f` can polymorph it's behaviour depending on the input type, thnaks to the interface blocks
   yi = f(xi)
   yr = f(xr)
   yz = f(xz)
   print *, yi, yr, yz
end program main