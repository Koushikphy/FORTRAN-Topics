module configParser
    ! generic function `parseVar` to parse variable of different kind
    ! supperted types are: integer, real64, logical, string, complex, array of integer and array of real64s
    ! defualt values can be set by passing as optional 4th argument, which will be used in case of missing the key
    ! ^defualt value not avilable for array type parsing
    ! Will thorw error, in case of missing key in config file if no default values are set
    interface parseVar
        module procedure parse_int, parse_real8, parse_logical, parse_string, parse_complex, parseIntArray, parseReal8Array
    end interface
contains


    subroutine parseIntArray(fUnit, varText, arr)
        integer :: fUnit, ios
        integer, allocatable :: arr(:)
        character(len=*) :: varText 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit) ! start looking from top
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                allocate(arr(countSubStr(val)))
                read(val,*) arr
            endif
        enddo
    end

    subroutine parseReal8Array(fUnit, varText, arr)
        integer :: fUnit, ios
        real(kind=8), allocatable :: arr(:)
        character(len=*) :: varText 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit) ! start looking from top
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                allocate(arr(countSubStr(val)))
                read(val,*) arr
            endif
        enddo
    end

    function countSubStr(str) result(nSubStr) !total length of the array, from counting occurrence of comma(',')
        character(len=*) :: str 
        integer :: nSubStr, lenTrim 
        str = adjustl(str)
        lenTrim = len_trim(str)
        if (str(lenTrim:lenTrim) == ',') stop "Bad input: Remove trailing comma(',')."
        nSubStr = count([(str(i:i), i=1,lenTrim)]==',')+1
    end


    subroutine parse_int(fUnit, varText, var, value)
        integer :: fUnit, ios
        character(len=*) :: varText 
        integer :: var 
        integer,optional :: value 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit) ! start looking from top
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        if(present(value)) then 
            var = value
        else 
            call error(varText)
        endif

    end

    subroutine parse_real8(fUnit, varText, var, value)
        integer :: fUnit, ios
        character(len=*) :: varText 
        real(kind=8) :: var 
        real(kind=8),optional :: value
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        if(present(value)) then 
            var = value
        else 
            call error(varText)
        endif

    end

    subroutine parse_logical(fUnit, varText, var, value)
        integer :: fUnit, ios
        character(len=*) :: varText 
        logical :: var 
        logical,optional :: value
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        if(present(value)) then 
            var = value
        else 
            call error(varText)
        endif

    end

    subroutine parse_string(fUnit, varText, var, value)
        integer :: fUnit, ios
        character(len=*) :: varText, var 
        character(len=*), optional :: value
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line/blank line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                var = val
                return
            endif
        enddo
        if(present(value)) then 
            var = value
        else 
            call error(varText)
        endif
    end

    
    subroutine parse_complex(fUnit, varText, var, value)
        integer :: fUnit, ios
        character(len=*) :: varText 
        complex(kind=8) :: var 
        complex(kind=8),optional :: value 
        character(len=100) :: lineTxt, key, val
        rewind(fUnit)
        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                read(val, *) var
                return
            endif
        enddo
        if(present(value)) then 
            var = value
        else 
            call error(varText)
        endif
    end

    subroutine getValuePair(line, key, val)
        character(len=100) :: line, key, val
        integer :: start, com
        start = index( line, '=')
        com = index(line, '#')
        if (com/=0) line = line(:com-1)  ! inline comment found, trim it

        key = line(:start-1)
        key = toLowerCase(key)
        val = adjustl(line(start+1:))
    end subroutine

    subroutine error(varText)
        character(len=*) :: varText 
        stop "Keyword '"//varText// "' not found in the config file"
    end

    function ignoreLine(txt) 
        ! comment or blank line
        character(len=*) :: txt
        logical :: ignoreLine
        txt = adjustl(txt)
        ignoreLine = .false.
        if(txt(:1)=="#" .or. len_trim(txt)==0) ignoreLine = .true.
    end

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
    integer :: a,aa
    real(kind=8) :: b 
    logical :: c
    character(len=100):: d
    complex(kind=8) :: e
    integer , allocatable :: f(:)
    real(kind=8) , allocatable :: g(:)


    open(fUnit, file='./abc.config',status='old',action='read')
    call parseVar(fUnit, 'a', a) ! parse integer
    call parseVar(fUnit, 'aa', aa, 66)  ! parse integer with a default value, defualt values are passed as optional 4th argument
    call parseVar(fUnit, 'b', b) ! parse real64
    call parseVar(fUnit, 'c', c) ! parse logical
    call parseVar(fUnit, 'd', d) ! parse string
    call parseVar(fUnit, 'e', e) ! parse conmplex
    call parseVar(fUnit, 'f', f) ! parse array of integer, variable has to be allocatable
    call parseVar(fUnit, 'g', g) ! parse array of real64, 
    print *, a
    print *,aa
    print *,b
    print *,c
    print *,d
    print *,e
    print *, f
    print *, g
end program name