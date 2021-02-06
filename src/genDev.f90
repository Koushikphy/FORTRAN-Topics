module example_module
    implicit none
    !https://stackoverflow.com/questions/20436475/interfaced-type-bound-procedures-in-fortran
    ! `add` is a genric function to function `add_integer` and `add_real` for the derived type `foo`
    type foo
        real  :: x=4.0
        contains
        generic, public :: add => add_integer,add_real
        procedure,private :: add_integer,add_real
    end type foo

    contains
    subroutine add_integer(self,x)
        class(foo), intent(in) :: self
        integer, intent(in) :: x
        print *,self%x+x
    end subroutine add_integer

    subroutine add_real(self,x)
        class(foo), intent(in) :: self
        real, intent(in) :: x
        print *,self%x+x
    end subroutine add_real
end module example_module


program example
    use example_module
    implicit none
    type(foo) :: foofoo
    call foofoo%add(1)
    call foofoo%add(2.0)

end program example