program hello_world
use mpi
integer ierr, num_procs, my_id, resultlen!, x
character*30 ::   name
call MPI_INIT ( ierr )
call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
call MPI_COMM_SIZE (MPI_COMM_WORLD, num_procs, ierr)
call MPI_GET_PROCESSOR_NAME(name, resultlen, ierr)

! call MPI_Bcast( x, 1, MPI_INT, 0, mpi_comm_world,ierr);

print *, "Hello world, from process no. ", my_id, ' running in host ', name
call MPI_FINALIZE ( ierr )
end program