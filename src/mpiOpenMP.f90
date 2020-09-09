program name
    use mpi
    use omp_lib
    character*30 :: pName

    call MPI_INIT(ierr)                                 ! invoke MPI, all below this will be run on every process
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)     ! get the currnet prcess rank, starts with 0, 0 is the master process
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr) ! get total number of processes spawn
    call MPI_GET_PROCESSOR_NAME(pName, nresLen, ierr)   ! get name of the host for current process

    ! each process now splits into OpenMP threads
    !$omp parallel do
    do i=1,omp_get_max_threads() ! <--splits into maximum threads
        write(111,"('Hello World from process: ',i3, ' on ', a, ',  thread: ', i3)") my_id,trim(pName), omp_get_thread_num() ! get currnt thread id
    enddo
    !$omp end parallel do

    call MPI_FINALIZE(ierr) ! join MPI tasks
    ! all MPI subroutines in fortran, has one error status `ierr` returned.
end program name