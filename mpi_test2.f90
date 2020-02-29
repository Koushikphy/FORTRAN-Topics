program hello_world
use mpi
integer ierr, num_procs, my_id, resultlen!, x
character*30 ::   name

! print *, "Hi there"
call MPI_INIT ( ierr )
call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
call MPI_COMM_SIZE (MPI_COMM_WORLD, num_procs, ierr)
call MPI_GET_PROCESSOR_NAME(name, resultlen, ierr)

print *, "Hello world, from process no. ", my_id, ' running in host ', name



! MASTER START
IF(my_id==0) print *, ' This is master thread'

call MPI_FINALIZE ( ierr )
end program