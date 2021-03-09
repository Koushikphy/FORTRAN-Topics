program name
    use iso_c_binding
    implicit none

    interface
        subroutine test(inp, out, n) bind(c,name= 'format_time')
            use iso_c_binding
            character(kind=c_char),intent(in) :: inp(*)
            integer(kind=c_int),intent(out) :: n
            type(c_ptr),intent(out) :: out
        end
    end interface


    character(kind=c_char, len=50), pointer :: timestr
    type(c_ptr) :: tt
    integer(c_int) :: nn

    call test("%B"//c_null_char, tt, nn)
    call c_f_pointer(tt,timestr)
    print *, timestr !<-- prints junk
    print*, nn

end program name