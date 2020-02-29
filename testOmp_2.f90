program name2
    !$use omp_lib
    implicit real*8(a-h,p-z)
    call system_clock(count_rate=irate)
    call system_clock(it1)
    !$ call omp_set_nested(.true.)
    !$ call omp_set_dynamic(.true.)
    !$omp parallel do num_threads(4)
    do i =1,5
        call test(i)
        print *, i
    enddo
    !$omp end parallel do 
    call system_clock(it2)
    print *, (it2-it1)/real(irate, kind=8)
end program name2


subroutine test(ii)
    ! just a dummy subroutine for heavy computation
    implicit real*8(a-h,p-z)
        !$omp parallel do num_threads(4)
        do j=1,5000
            do k=1,50000
                x = exp(sqrt(sqrt(2.0d0*ii**3)**2))
            enddo
        enddo
        !$omp end parallel do 
end subroutine
