! matrix multiplication without any loops, using recursive algorithm

program name
    implicit none
    integer, parameter :: n=3
    integer :: x(n,n), y(n,n), z(n,n)
    integer :: m
    x = reshape([(m,m=1,n**2)],[n,n])
    y = x 
    call mat(x,y,n,z)
end program name


recursive subroutine mat(x,y,n, z)
    integer, intent(in) :: n, x(n,n), y(n,n)
    integer, intent(out) :: z(n,n)
    integer :: i=1, j=1, k=1

    if(i==1 .and. j==1 .and. k==1) z=0    ! initialize z

    z(i,j) = z(i,j) + x(i,k)*y(k,j)

    k=k+1
    if(k>n) then
        k=1
        j = j+1
    endif
    
    if(j>n) then
        j=1
        i = i+1
    endif 
    if(i>n) return


    call mat(x,y,n,z)
end 