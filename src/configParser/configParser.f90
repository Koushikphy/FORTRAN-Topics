module configParser
    ! generic function `parseVar` to parse variable of different kind
    ! supported types are: integer, real64, logical, string, complex, array of integer and array of real64s
    ! defualt values can be set by passing as optional 4th argument, which will be used in case of missing the key
    ! Will thorw error, in case of missing key in config file if no default values are set
    implicit none
    type config
        logical :: fileOpened = .false.
        integer :: fileUnit
        contains
        procedure :: openConfig, checkFile
        procedure, private:: parse_int, parse_real8, parse_logical, parse_string, parse_complex,parseIntArray,parseReal8Array
        generic :: parse => parse_int, parse_real8, parse_logical, parse_string, parse_complex,parseIntArray,parseReal8Array
    end type config

    contains


    subroutine openConfig(self, fName)
        class(config) :: self
        character(len=*) :: fName
        open(newunit=self%fileUnit, file=trim(fName), status='old', action='read')
        self%fileOpened = .true.
    end

    subroutine checkFile(self)
        class(config) :: self
        if(.not. self%fileOpened) then
            write(*,*) "No config file is opened"
            stop
        endif
    end


    subroutine parse_int(self, varText, var, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        character(len=*) :: varText
        integer :: var
        integer,optional :: value
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
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


    subroutine parse_real8(self, varText, var, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        character(len=*) :: varText
        real(kind=8) :: var
        real(kind=8),optional :: value
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
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

    subroutine parse_logical(self, varText, var, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        character(len=*) :: varText
        logical :: var
        logical,optional :: value
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
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

    subroutine parse_string(self, varText, var, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        character(len=*) :: varText, var
        character(len=*), optional :: value
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
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


    subroutine parse_complex(self, varText, var, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        character(len=*) :: varText
        complex(kind=8) :: var
        complex(kind=8),optional :: value
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
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

    subroutine parseIntArray(self, varText, arr, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        integer, allocatable :: arr(:)
        integer, intent(in), optional :: value(:)
        character(len=*) :: varText
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
        rewind(fUnit) ! start looking from top

        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                allocate(arr(countSubStr(val)))
                read(val,*) arr
                return
            endif
        enddo
        if(present(value))then
            arr = value
        else
            call error(varText)
        endif
    end

    subroutine parseReal8Array(self, varText, arr, value)
        class(config), intent(in) :: self
        integer :: fUnit, ios
        real(kind=8), allocatable :: arr(:)
        real(kind=8), intent(in), optional :: value(:)
        character(len=*) :: varText
        character(len=100) :: lineTxt, key, val

        call self%checkFile()
        fUnit = self%fileUnit
        rewind(fUnit) ! start looking from top

        do
            read(fUnit, '(a)',iostat=ios) lineTxt
            if(ios /=0) exit
            if(ignoreLine(lineTxt)) cycle  ! commented line ignore
            call getValuePair(lineTxt, key, val)
            if(key==varText) then
                allocate(arr(countSubStr(val)))
                read(val,*) arr
                return
            endif
        enddo
        if(present(value))then
            arr = value
        else
            call error(varText)
        endif

    end

    function countSubStr(str) result(nSubStr) !total length of the array, from counting occurrence of comma(',')
        character(len=*) :: str
        integer :: nSubStr, lenTrim,i
        str = adjustl(str)
        lenTrim = len_trim(str)
        if (str(lenTrim:lenTrim) == ',') stop "Bad input: Remove trailing comma(',')."
        nSubStr = count([(str(i:i), i=1,lenTrim)]==',')+1
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
        character(len=*), intent(in) :: varText
        write(*,*) "Keyword '"//varText// "' not found in the config file"
        stop
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

end module



program name
    use configParser
    implicit none
    type(config) :: tt 

    integer :: a,aa
    real(kind=8) :: b 
    logical :: c
    character(len=100):: d
    complex(kind=8) :: e
    integer , allocatable :: f(:), ff(:)
    real(kind=8) , allocatable :: g(:)


    call tt%openConfig('./abc.config')


    call tt%parse( 'a', a) ! parse integer
    call tt%parse( 'aa', aa, 66)  ! parse integer with a default value, defualt values are passed as optional 4th argument
    call tt%parse( 'b', b) ! parse real64
    call tt%parse( 'c', c) ! parse logical
    call tt%parse( 'd', d) ! parse string
    call tt%parse( 'e', e) ! parse conmplex
    call tt%parse( 'f', f) ! parse array of integer, variable has to be allocatable
    call tt%parse( 'ff', ff, [1,2,99]) ! parse array of integer, variable has to be allocatable
    call tt%parse( 'g', g) ! parse array of real64, 


    print *, a
    print *,aa
    print *,b
    print *,c
    print *,d
    print *,e
    print *, f
    print *, ff
    print *, g


end program name