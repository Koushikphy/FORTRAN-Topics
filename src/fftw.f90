module fftw
    integer, parameter :: FFTW_R2HC                   = 0
    integer, parameter :: FFTW_HC2R                   = 1
    integer, parameter :: FFTW_DHT                    = 2
    integer, parameter :: FFTW_REDFT00                = 3
    integer, parameter :: FFTW_REDFT01                = 4
    integer, parameter :: FFTW_REDFT10                = 5
    integer, parameter :: FFTW_REDFT11                = 6
    integer, parameter :: FFTW_RODFT00                = 7
    integer, parameter :: FFTW_RODFT01                = 8
    integer, parameter :: FFTW_RODFT10                = 9
    integer, parameter :: FFTW_RODFT11                = 10
    integer, parameter :: FFTW_FORWARD                = -1
    integer, parameter :: FFTW_BACKWARD               = +1
    integer, parameter :: FFTW_MEASURE                = 0
    integer, parameter :: FFTW_DESTROY_INPUT          = 1
    integer, parameter :: FFTW_UNALIGNED              = 2
    integer, parameter :: FFTW_CONSERVE_MEMORY        = 4
    integer, parameter :: FFTW_EXHAUSTIVE             = 8
    integer, parameter :: FFTW_PRESERVE_INPUT         = 16
    integer, parameter :: FFTW_PATIENT                = 32
    integer, parameter :: FFTW_ESTIMATE               = 64
    integer, parameter :: FFTW_ESTIMATE_PATIENT       = 128
    integer, parameter :: FFTW_BELIEVE_PCOST          = 256
    integer, parameter :: FFTW_NO_DFT_R2HC            = 512
    integer, parameter :: FFTW_NO_NONTHREADED         = 1024
    integer, parameter :: FFTW_NO_BUFFERING           = 2048
    integer, parameter :: FFTW_NO_INDIRECT_OP         = 4096
    integer, parameter :: FFTW_ALLOW_LARGE_GENERIC    = 8192
    integer, parameter :: FFTW_NO_RANK_SPLITS         = 16384
    integer, parameter :: FFTW_NO_VRANK_SPLITS        = 32768
    integer, parameter :: FFTW_NO_VRECURSE            = 65536
    integer, parameter :: FFTW_NO_SIMD                = 131072
    integer, parameter :: FFTW_NO_SLOW                = 262144
    integer, parameter :: FFTW_NO_FIXED_RADIX_LARGE_N = 524288
    integer, parameter :: FFTW_ALLOW_PRUNING          = 1048576
    integer, parameter :: FFTW_TIMELIMIT              = 1073741824
end module fftw


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