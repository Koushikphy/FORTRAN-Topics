program name
    !$use omp_lib
    implicit real*8(a-h,p-z)
    call system_clock(count_rate=irate)
    call system_clock(it1)
    !$ call omp_set_nested(.true.)
    !$omp parallel
    !$omp single
    do i =1,5
        !$omp task
        call test(i)
        print *, i
        !$omp end task
    enddo
    !$omp end single
    !$omp end parallel
    call system_clock(it2)
    print *, (it2-it1)/real(irate, kind=8)
end program name



subroutine test(ii)
    ! just a dummy subroutine for heavy computation
    implicit real*8(a-h,p-z)
        do j=1,5000
            !$omp task
            do k=1,50000
                x = exp(sqrt(sqrt(2.0d0*ii**3)**2))
            enddo
            !$omp end task
        enddo
end subroutine








