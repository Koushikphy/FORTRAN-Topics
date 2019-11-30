program pointer
  real,target :: x, xx(10), xxx(5,2)
  real, pointer :: y, yy(:), yyy(:)
  integer :: i
  y  => x
  yy => xx
  yyy => xxx(:,1)  ! associate the first column
  x  = 91
  xx = [(i, i=1,10)]
  print *, x, y*2, char(10)
  y=y*2 ! this should change x also
  print *, x
  print *, xx,char(10), yy
  
  
  
  xxx = reshape(xx,[5,2], order=[2,1])
  yyy = yyy*5 
  print *,'xxx=>', xxx


  print *, associated(y) ! check assoication
  print *, associated(y, target=x)
  nullify(y,yy)  ! clear association
  print *, associated(y)

!   x = 10; xx=10
!   print *, x, y, char(10)
!   print *, xx,char(10), yy
end program pointer