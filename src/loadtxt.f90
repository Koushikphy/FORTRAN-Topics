module fileUtil
    contains
    subroutine loadtxt(fileName, data)
        character(len=*), intent(in) :: fileName
        real(kind=8), allocatable,intent(out) :: data(:,:)
        integer :: n, ios, nCol,nRow,i
        logical ::  lastWhite, thisWhite
        character :: c
        real(kind=8) :: r
        open(newunit=n, file=fileName, status='old', action='read')
    
        nCol = 0 ; nRow=0; lastWhite=.true.

        do
            read(n, '(a)', advance='no', iostat=ios) c
            if (ios /= 0) exit
            thisWhite = iachar(c) == 32 .or. iachar(c) == 9 ! white space or tab
            if (lastwhite .and. .not. thisWhite) ncol = ncol + 1
            lastwhite = thisWhite
        end do
        rewind(n)
        do
           read(n, *, iostat=ios) r
           if (ios /= 0) exit
           nrow = nrow + 1
        end do
        rewind(n)
    
        allocate(data(nRow,nCol))
        do i = 1, nrow
            read(n, *) data(i, :)
        end do
        close(n)
    end


    subroutine savetxt(filename, data)

        character(len=*), intent(in) :: filename  ! File to save the array to
        real(kind=8), intent(in) :: data(:, :)           ! The 2D array to save

        integer :: n, i
        open(newunit=n, file=filename, status="replace")
        do i = 1, size(data, 1)
            write(n, *) data(i, :)
        end do
        close(n)
        end subroutine
end


program name
    use fileUtil
    implicit none

    real(kind=8), allocatable :: data(:,:)

    call loadtxt('./data.dat', data)
    call savetxt('tmp.dat', data)
    print *, data
end program name