! read an string from the command line argument

program name
    character(len=100):: fName

    n= command_argument_count() ! number of given arguments

    if(n==0) then ! no argument is provided use some defualt name
        fName = "Hi there"
    else 
        call get_command_argument(1,fName) ! read argument at position 1
    endif
    
    print *, fName

end program name