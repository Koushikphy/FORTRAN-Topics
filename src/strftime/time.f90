! strftime reference : https://www.cplusplus.com/reference/ctime/strftime/
module timeStamp
    use iso_c_binding, only : c_char, c_ptr, c_null_char, c_f_pointer
    implicit none
    interface
        function get(form) result(txt) bind(c, name='format_time')
            import :: c_char, c_ptr
            character(kind=c_char),intent(in):: form(*)
            type(c_ptr):: txt
        end
    end interface

    contains
    function strftime(format) result(timestr)
        character(len=*) :: format
        character(kind=c_char, len=100), pointer :: timestr
        call c_f_pointer( get(format//c_null_char), timestr)
    end
end module 

program name
    use timeStamp
    implicit none
    print *, strftime("%A %d-%B, %Y %I:%M:%S %p")
end program name
