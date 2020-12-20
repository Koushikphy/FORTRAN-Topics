module configParser
    interface parseVar
        module procedure parse_int, parse_real8, parse_logical, parse_string, parse_complex
    end interface
contains

    subroutine parse_int(fUnit, varText, var)
        integer :: fUnit, ios
        character(len=*) :: varText 
        integer :: var 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios)lineTxt
            if(ios /=0) exit
            lineTxt = adjustl(lineTxt)
            if(lineTxt(:1)=="#" .or. len_trim(lineTxt)==0) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        stop "Keyword '"//varText// "' not found in the config file"
    end

    subroutine parse_real8(fUnit, varText, var)
        integer :: fUnit, ios
        character(len=*) :: varText 
        real(kind=8) :: var 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios)lineTxt
            if(ios /=0) exit
            lineTxt = adjustl(lineTxt)
            if(lineTxt(:1)=="#" .or. len_trim(lineTxt)==0) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        stop "Keyword '"//varText// "' not found in the config file"
    end

    subroutine parse_logical(fUnit, varText, var)
        integer :: fUnit, ios
        character(len=*) :: varText 
        logical :: var 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios)lineTxt
            if(ios /=0) exit
            lineTxt = adjustl(lineTxt)
            if(lineTxt(:1)=="#" .or. len_trim(lineTxt)==0) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        stop "Keyword '"//varText// "' not found in the config file"
    end

    subroutine parse_string(fUnit, varText, var)
        integer :: fUnit, ios
        character(len=*) :: varText, var 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios)lineTxt
            if(ios /=0) exit
            lineTxt = adjustl(lineTxt)
            if(lineTxt(:1)=="#" .or. len_trim(lineTxt)==0) cycle  ! commented line/blank line ignore
            ! print*, lineTxt
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                var = val
                return
            endif
        enddo
        stop "Keyword '"//varText// "' not found in the config file"
    end

    
    subroutine parse_complex(fUnit, varText, var)
        integer :: fUnit, ios
        character(len=*) :: varText 
        complex(kind=8) :: var 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios)lineTxt
            if(ios /=0) exit
            lineTxt = adjustl(lineTxt)
            if(lineTxt(:1)=="#" .or. len_trim(lineTxt)==0) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                print*, val
                read(val, *) var
                return
            endif
        enddo
        stop "Keyword '"//varText// "' not found in the config file"
    end

    subroutine getValuePair(line, key, val)
        character(len=100) :: line, key, val
        integer :: start, com
        start = index( line, '=')
        com = index(line, '#')
        if (com/=0) line = line(:com-1)  ! inline comment found

        if(start==0) stop 'Invalid format for keyword'
        key = line(:start-1)
        key = toLowerCase(key)
        val = adjustl(line(start+1:))
    end subroutine


    function toLowerCase(str)
        ! for convenience all string will be  will be converted to lowercase for checking
        character(len=100):: str, toLowerCase
        integer :: n, i , icc
        n = len_trim(str)
        do i = 1,n
            icc = iachar(str(i:i))
            if(65<=icc .and. icc<=90) str(i:i) = achar(icc+32)
        enddo
        toLowerCase = str
    end
end

program name
    use configParser
    implicit none
    integer :: fUnit
    integer :: a 
    real(kind=8) :: b 
    logical :: c
    character(len=100):: d
    complex(kind=8) :: e
    open(fUnit, file='./abc.config',status='old',action='read')
    call parseVar(fUnit, 'd', d)
    print *, d
end program name