program name
    use iso_c_binding, only: c_float
    implicit none
    ! C uses pass-by-value, while Fortran uses pass-by-reference
    ! By default the linked Fortran function/subroutine will pass the values as reference, so in `C` side
    ! you have to use it as pointer. Check the `myFuncRef` function in c and `cFuncRef` interface
    ! Otherwise we can tell Fortran to pass it by value. Check `myFunc` and `cFunc`

    ! By default, a Fortran function/subroutine call will be seen by `C` as function call represented by pointer,
    ! with an added underscore (_) sign, so instead of defining a interface we can just call the c function quickly 
    ! as an external function. Check the `myFuncAuto` function (Check the letter cases between the C vs Fortran call )
    interface
        function cFunc(x,y) result(z) bind(c, name='myFunc') ! binding name is case sensetive
            import :: c_float
            real(c_float),value :: x,y
            real(c_float) :: z
        end

        function cFuncRef(x,y) result(z) bind(c, name='myFuncRef') ! binding name is case sensetive
            import :: c_float
            real(c_float) :: x,y
            real(c_float) :: z
        end
    end interface
    
    real(c_float),external:: myFuncAuto

    print *, cFunc(1.0,99.0)
    print *, cFuncRef(1.0,99.0)
    print *, myFuncAuto(1.0,99.0)
end program name