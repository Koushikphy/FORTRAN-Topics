program main
integer,parameter :: dp = kind(0.d0)
integer,parameter :: dp1 = selected_real_kind(16,30)
complex(kind=8)::a
double complex :: c
real(kind=dp1)::b

  ! m(:)=array(1,:)
  ! ! n(:)=array(2,:)
  ! ! write(*,*)m, pack(m, m>4)
  ! ! where(m>2)
  ! !   m=7
  ! ! endwhere
  ! ! new = 9999
  ! ! arr(2:3,:)=new
  ! newarr(1,1)=999
  ! write(*,*)arrr
  ! write(*,*)newarr
  ! forall(i=1:3) m(i)=99
  a = complex(1,1)
  print *,huge(real(a)), huge(real(c)), huge(b)
end program main