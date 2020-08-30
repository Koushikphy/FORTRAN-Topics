! User defined derived data types, similar to c structure or object attribute
program test
    type point
      real :: x
      real :: y
      real :: z
    end type point
    type(point) :: pt1, pt2
    real          :: offset

    pt2%x = 10
    pt2%y = 20
    pt2%z = 30
    offset = 100.0

    pt1 = pt2
    print *, pt1
    print *, pt1%x, pt1%y, pt1%z
end program test