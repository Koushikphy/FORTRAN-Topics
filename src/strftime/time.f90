! strftime reference : https://www.cplusplus.com/reference/ctime/strftime/
module timeStamp
    use iso_c_binding
    implicit none 
    interface
        subroutine get(inp, out, n) bind(c,name= 'format_time')
            import :: c_char, c_int, c_ptr
            character(kind=c_char) :: inp(*), out(*)
            integer(kind=c_int),intent(out) :: n
        end
    end interface

    contains
    function strftime(format) result(txt)
        character(len=*) :: format
        character(kind=c_char, len=100) :: txt
        integer(c_int) :: n
        n=100 ! on input `n` is size of the output string, on output its length of the string
        call get(format//c_null_char, txt, n)
        txt(n+1:) = ''
    end
end module

program name
    use timeStamp
    write(11,*) strftime('%x-%X')
end program name