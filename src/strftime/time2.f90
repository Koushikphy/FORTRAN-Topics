! Unlike the other it uses passes a pointer to a string to the c side.
module timeStamp
    use iso_c_binding
    implicit none

    interface
        subroutine test(inp, out, n) bind(c,name= 'format_time')
            import :: c_char, c_int, c_ptr
            character(kind=c_char),intent(in) :: inp(*)
            integer(kind=c_int),intent(out) :: n
            type(c_ptr),intent(out) :: out
        end
    end interface


    contains
    function strftime(format) result(txt)
        character(len=*), intent(in) :: format
        character(kind=c_char, len=50), pointer :: txt
        type(c_ptr) :: tt
        integer(c_int) :: nn
        
        call test(format//c_null_char, tt, nn)
        call c_f_pointer(tt,txt)
        txt(nn+1:) =''
    end
end






program name

    use timeStamp
    write(11,*) strftime('%x-%X')

end program name