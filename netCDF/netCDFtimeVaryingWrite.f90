! example of netCDF to write a 2D data that is being generated over a unknown time steps
! gfortran timeVaryingArray.f90 -I/usr/include/ -lnetcdff && ./a.out
! sudo apt-get install libnetcdf-dev libnetcdff-dev


program name
    use netcdf
    implicit none
    character(len=*), parameter :: fileName = 'myfile.nc'

    ! suppose a array of dimension (nx,ny) is varying over time, maybe a z of 
    ! some function z=f(x,y;t) that varying over time
    integer,parameter :: nDims = 2, nX = 5, nY = 5
    integer:: dimids(nDims+1), start(nDims+1)  ! +1 is for time dimesion
    integer,parameter :: timeSteps = 3 ! not required at declaration time
    real(kind=8) :: dat(nX,nY)

    !ids
    integer :: x_dimid, y_dimid,data_dimid,x_varid,y_varid,data_id,ncid,i
    ! all of these nf90_* define functions retruns some `id` at the last argument
    ! and all of the funcitons return status as result
    ! pass the return status to check to verify the return
    call check( nf90_create(fileName, IOR(NF90_NETCDF4, NF90_CLOBBER), ncid) )  ! `clobber` removes any existing file
    ! or nf90_open

    ! define dimension information to store the variables, have to define for each dimensions
    call check( nf90_def_dim(ncid, "x", nX, x_dimid) )  ! xaxis
    call check( nf90_def_dim(ncid, "y", nY, y_dimid) ) !yaxis
    ! time dimension.. this should be unlimited, if we don't know the time beforehaand, so that the file
    ! can grow to accomodate new timestep data, 
    call check( nf90_def_dim(ncid, "time", NF90_UNLIMITED, data_dimid) ) 
    !you can, use `nccopy -u file1 file2` afterwards to convert from unlimited to fixed, this reduces size
    
    
    ! specify the variable info, [nx_dimid] can be replaced with simply `nx_dimid` as its one dimension
    call check( nf90_def_var(ncid, "x_grid", NF90_DOUBLE, [x_dimid], x_varid   ))
    call check( nf90_def_var(ncid, "y_grid", NF90_DOUBLE, [y_dimid], y_varid   ))
    call check( nf90_def_var(ncid, "My_Data", NF90_DOUBLE, [ x_dimid, y_dimid, data_dimid], data_id, deflate_level =1) )
    ! deflate level is compression, unlimited dimension degrades compression
    ! also use chunksize and shuffle, check with `nccopy utility`



    ! put some attributes/metadata of the variable, not mandatory
    
    call check( nf90_put_att(ncid, x_varid, "message", "full description") )
    call check( nf90_put_att(ncid, y_varid, "message", "full description") )
    call check( nf90_put_att(ncid, data_id, "message", "full description") )


    call check( nf90_enddef(ncid) )
    ! end of definition mode, now file is ready to write data


    ! DATA WRITE START======>

    ! strore the grid data, this is one time
    call check( nf90_put_var(ncid, x_varid, [1,2,3,4,5]) )
    call check( nf90_put_var(ncid, y_varid, [5,4,3,2,1]) )


    ! write data
    do i=1,1000
        ! start means where to start to put the data in each dimension, 
        ! and count means how much if each dimension to fill
        ! a particular timestep data obviously will span the whole x,y grid but a single point in time dimension
        ! thus the value of `count`; and start(3) i.e. time coordinate will change in each timestep
        call random_number(dat) ! just some random data to fill
        dat = i
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