! example of netCDF to append a 2D data that is being generated over a unknown time steps, to the companyon file that is already present
! from the write file
! gfortran timeVaryingArray.f90 -I/usr/include/ -lnetcdff && ./a.out
! sudo apt-get install libnetcdf-dev libnetcdff-dev


program name
    use netcdf
    implicit none
    character(len=*), parameter :: fileName = 'myfile.nc'

    integer,parameter :: nDims = 2, nX = 5, nY = 5
    real(kind=8) :: dat(nX,nY)

    integer :: data_id,ncid,i

    !open the existing file...
    call check( nf90_open(fileName, NF90_WRITE, ncid) ) 

    ! get the data location
    call check( nf90_inq_varid(ncid, "My_Data", data_id) )
    ! and write...

    ! write data
    do i=2,4  ! starting from 2, so `1` data should stay unchanged
        dat = i*.10d0
        call check( nf90_put_var(ncid, data_id, dat, start = [1,1,i], count = [nx,ny,1]) )
        call check( nf90_sync(ncid))  ! flush out data if needed
    enddo
    call check( nf90_close(ncid) )




    contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
  end subroutine check
end program name