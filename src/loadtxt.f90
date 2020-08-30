subroutine getcolumns(filename, len)
    ! returns number of colums in a space delimited data file
    character(len=*), intent(in) :: filename
    integer, intent(out) :: len 
    integer :: ff,k,i
    character(len=200) :: txt
    open(newunit=ff, file = trim(filename), status = 'old', action = 'read')
    read(ff,'( a )')txt
    close(ff)

    i = 0; k = 0; len = 0
    do
        i = index(trim(txt(k+1:)), ' ')
        len = len +1
        if(i==0) exit
        k = k + i
    enddo
end


subroutine getLines(filename, len)
    ! returns number of non-blank rows from a data file
    character(len=*), intent(in) :: filename
    character(len=50) ::cmd, tmpfile = 'tmp.dat' ! check if this file does not exist
    integer, intent(out) :: len
    integer :: ff

    ! run the bash command the value and store it inside a file, as fortran
    ! can't directly get the return value form bash

    cmd = 'cat '//trim(filename)//' | grep [0-9] | wc -l>'//trim(tmpfile)
    call system(cmd)

    open(newunit=ff, file = trim(tmpfile), status = 'old', action = 'read')
    read(ff,*)len
    close(ff)

end





program reading
    implicit none
  ! variable declaration
    real, dimension(:) ,allocatable :: x 
    real, dimension(:,:) ,allocatable :: y
    real :: dist ,x_tmp ,y_tmp, z(2)
    integer:: i, j ,n ,io, last, current, point, count 
    character(len=200) :: txt

    allocate(y(2,5))



    call getLines('test.txt',last)
    print *, last
    

    ! open(2, file = 'test.txt', status = 'old', action = 'read')
    ! read(2,*)y
    ! print *,y


    ! read(2,'( a )')txt
    ! print *, txt
    ! rewind(2)


    ! current = index(trim(txt), ' ', .true.)
    ! print *, index(trim(txt(9:)), ' ')
    ! current = 0
    ! point = 0
    ! count = 0
    ! do
    !     current = index(trim(txt(point+1:)), ' ')
    !     count = count +1
    !     if(current==0) exit
    !     point = point + current
    ! enddo

    ! print *, 'number of columns:',count


    ! allocate(x(2))
    ! do   
    !     Read( 2, * ,iostat=io) x
    !     IF (io/=0) EXIT
    !     print *, x
    ! enddo

    ! Do
    !     Read( 10, '( a )', End = 200 ) line
    !     ! Work out how many numbers I've just eaten
    !     numnum = Count( (/ ( line( i:i ), i = 1, Len( line ) ) /) == ',' )
    !     numnum = numnum + 1
    !     ! Turn the commas into spaces adn work out where the original line ends
    !     Do i = 1, Len( line )
    !        If( line( i:i ) == ' ' ) Then
    !           Exit
    !        End If
    !        If( line( i:i ) == ',' ) Then
    !           line( i:i ) = ' ' 
    !        End If
    !     End Do
    !     Write( *, * ) numnum, line( 1:Min( Len( line ), i ) )
    !  End Do


    ! n = 0
    ! allocate( x(0) ,y(0) )
    ! DO
    !   READ(2,*,iostat=io) x_tmp,y_tmp
    !   x = [x,x_tmp]
    !   y = [y,y_tmp]
    !   IF (io/=0) EXIT
    !   n = n + 1
    ! END DO

    ! print*, n 
    ! ! writing and saving 
    ! DO i= 1, n 
    !   write(*,*) i, x(i)
    ! END DO

  end program reading