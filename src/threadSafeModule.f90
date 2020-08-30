module test 
    integer,private :: val 
    !$omp threadprivate(val)
    contains
    integer function getVal()
        getVal = val
    end
    subroutine setVal(n)
        val=n
    end
end 


program name
    use test 
    
    !$omp parallel do private(i,j) 
    do i = 1,10
        call setVal(i) 
        !^- module internal values are set in parallel, so if they are not thread safe,
        !below `getVal` can get value set by some other thread
        do j = 1,10
            print *, i, j, getVal()
        enddo
    enddo
    !$omp end parallel do

end program name