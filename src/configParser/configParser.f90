module configParser
    !! A generic ConfigParser to parse key-value pair from config file.
    !! Supported types are: integer, real64, logical, string, complex, array of integer and array of real64s  
    !! Defualt values can be set by passing as optional 4th argument, which will be used in case of missing the key  
    !! Will thorw error, in case of missing key in config file if no default values are set  
    implicit none
    private
    type config
        !! Config parser object. 
        !! First call the `openConfig` to initialize the parser, then use the generic `parseVar` function to parse required variable
        logical,private :: fileOpened = .false.
        integer,private :: fileUnit
        contains
        procedure :: openConfig
        procedure,private :: checkFile, parse_int, parse_real8, parse_logical, parse_string, parse_complex,&
                             parseIntArray,parseReal8Array
        generic :: parse => parse_int, parse_real8, parse_logical, parse_string, parse_complex,parseIntArray,parseReal8Array
    end type config
    public :: config
    contains


    subroutine openConfig(self, fName)
        !! Open config file for parsing
        class(config),intent(inout) :: self
        character(len=*) :: fName
        open(newunit=self%fileUnit, file=trim(fName), status='old', action='read')
        self%fileOpened = .true.
    end


    subroutine checkFile(self)
        !! Check if the file is open for reading
        class(config),intent(in) :: self
        if(.not. self%fileOpened) then
            write(*,*) "No config file is opened"
            stop
        endif
    end


    subroutine parse_int(self, varText, var, value)
        !! Parse integer 
        class(config), intent(in)   :: self    !! config object
        character(len=*),intent(in) :: varText !! Variable key
        integer,intent(out)         :: var     !! Variable value
        integer,intent(in),optional :: value   !! Default optional value
        integer :: fUnit, ios
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
        !! Parse double precision real number 
        class(config), intent(in)        :: self   
        character(len=*),intent(in)      :: varText
        real(kind=8),intent(out)         :: var
        real(kind=8),intent(in),optional :: value
        integer :: fUnit, ios
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
        !! Parse logical variable
        class(config), intent(in)   :: self
        character(len=*),intent(in) :: varText
        logical,intent(out)         :: var
        logical,intent(in),optional :: value
        integer :: fUnit, ios
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
        !! Parse string variable
        class(config), intent(in)             :: self
        character(len=*),intent(in)           :: varText
        character(len=*),intent(out)          :: var
        character(len=*),intent(in), optional :: value
        integer :: fUnit, ios
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
        !! Parse complex number
        class(config), intent(in)             :: self
        character(len=*), intent(in)          :: varText
        complex(kind=8),intent(out)           :: var
        complex(kind=8), intent(in), optional :: value
        integer :: fUnit, ios
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
        !! Parse array of integers
        class(config), intent(in)        :: self
        character(len=*),intent(in)      :: varText
        integer,intent(out), allocatable :: arr(:)
        integer, intent(in), optional    :: value(:)
        integer :: fUnit, ios
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
        !! Parse array of real numbers
        class(config), intent(in)             :: self
        character(len=*),intent(in)           :: varText
        real(kind=8),intent(out), allocatable :: arr(:)
        real(kind=8), intent(in), optional    :: value(:)
        integer :: fUnit, ios
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


    function countSubStr(str) result(nSubStr) 
        !count the total length of the array, from counting occurrence of comma(',')
        character(len=*) :: str
        integer :: nSubStr, lenTrim,i
        str = adjustl(str)
        lenTrim = len_trim(str)
        if (str(lenTrim:lenTrim) == ',') stop "Bad input: Remove trailing comma(',')."
        nSubStr = count([(str(i:i), i=1,lenTrim)]==',')+1
    end


    subroutine getValuePair(line, key, val)
        ! get key-value pair from a line
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