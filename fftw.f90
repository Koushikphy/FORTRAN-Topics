program test
use fftw
! use omp_lib ! not necessary
implicit none
! -lfftw3 -lfftw3_omp -fopenmp
integer, parameter :: n=10000
real, parameter :: pi = 4.0*atan(1.0)
real:: a,b
complex(kind=8) :: x(n), y(n)  !< just for making plans
complex(kind=8) :: grid(n), sin2y(n), out(n)
integer :: i, iret
integer(kind=8) :: plan, plane! plans require integer of kind 8

call cpu_time(a)

! second harmonic will be there after fft
grid = [(i*2*pi/n, i=1,n)]
sin2y = sin(2*grid)

! print *, omp_get_num_procs()
! call omp_set_num_threads (4)

! call dfftw_init_threads(iret)
! call dfftw_plan_with_nthreads(4)
! fftw_estimate directly calculates the fft with a predefined algorithm (CT) 
! and does not modify the input arrays, so only executing the `dfftw_execute` is enough
call dfftw_plan_dft_1d(plane,n,sin2y,out,FFTW_FORWARD,FFTW_ESTIMATE)
call dfftw_execute(plane)

do i=1,n
    write(102,*) real(sin2y(i)),real(out(i))
enddo


! on the other hand fftw_measure, _patient, _exhaustive etc. uses different 
! technique to check the best algorithm for the given input and output array
! NOTE: These methods modifies the `x` and `y`, so executing with a
! explicit array is required
print *, sum(x)
call dfftw_plan_dft_1d(plan,n,x,y,FFTW_FORWARD,ior(FFTW_MEASURE, FFTW_DESTROY_INPUT))
call dfftw_plan_dft_1d(plane,n,x,y,FFTW_BACKWARD,ior(FFTW_MEASURE, FFTW_DESTROY_INPUT))
print *, sum(x)

call dfftw_execute_dft(plan, sin2y, out)
call cpu_time(b)
print *, b-a
do i=1,n
    write(101,*) real(sin2y(i)),real(out(i))
enddo

end program test