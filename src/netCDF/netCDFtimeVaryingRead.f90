! example of netCDF to read a 2D data that is generated over a time steps
! companyon program for `netCDFtimeVaryingWrite.f90`


program name
    use netcdf
    ! implicit none
    character(len=*), parameter :: fileName = 'myfile.nc'

    integer :: x_varid,y_varid,data_varid,ncid,i
    integer,parameter :: nDims = 2, nX = 5, nY = 5
    integer:: dimids(nDims+1), start(nDims+1)  ! +1 is for time dimesion
    integer,parameter :: timeSteps = 3 
    real(kind=8) :: dat(nX,nY), xx(nX), yy(nY)


    call check( nf90_open(fileName, nf90_nowrite, ncid) )  ! 


  ! Get the varids of the latitude and longitude coordinate variables.
    call check( nf90_inq_varid(ncid, "x_grid", x_varid) )
    call check( nf90_get_var(ncid, x_varid, xx) )
    write(111,*)xx 
    write(111,*)


    call check( nf90_inq_varid(ncid, "y_grid", y_varid) )
    call check( nf90_get_var(ncid, y_varid, yy) )
    write(111,*)yy
    write(111,*)

    call check( nf90_inq_varid(ncid, "My_Data", data_varid) )


    ! read data, one time step at a time
    do i=1,timeSteps
      call check( nf90_get_var(ncid, data_varid, dat, start = [1,1,i], count = [nx,ny,1]) )
        write(111,*)dat
        write(111,*)
    enddo
    call check( nf90_close(ncid) )




    contains
  subroutine check(status)
    integer, intent ( in) :: status
    integer :: dum
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      dum = nf90_close(ncid) 
      stop 2
    end if
  end subroutine check
end program name