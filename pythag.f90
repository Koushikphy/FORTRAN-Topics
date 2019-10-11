!  finds sqrt(a**2+b**2) without overflow (i.e no number greater than a or b is calculated anywhere in the calculation) 
!  or destructive underflow


! REAL*8 FUNCTION PYTHAGo(A,B)
!       implicit real*8(a-h,o-z)
!     !  finds sqrt(a**2+b**2) without overflow or destructive underflow
!       P = dMAX1(dABS(A),daBS(B))
!       IF (P .EQ. 0.0d0) GO TO 20
!       R = (dMIN1(dABS(A),dABS(B))/P)**2
!    10 CONTINUE
!          T = 4.0d0 + R
!          IF (T .EQ. 4.0d0) GO TO 20
!          S = R/T
!          U = 1.0d0 + 2.0d0*S
!          P = U*P
!          R = (S/U)**2 * R
!       GO TO 10
!    20 PYTHAG = P
!       RETURN
!       END


real*8 function pythag(a,b) result(p)
    real*8, intent(in):: a,b 
    real*8:: q ,r,s
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))

    do while(q>1e-10)
        r = (q/p)**2
        s = r/(4+r)
        p = p + 2*s*p 
        q = s*q
    enddo
    ! do
    !     r = (q/p)**2
    !     s= r+4
    !     if (s .eq. 4.0d0) exit
    !     s = r/s
    !     p = p + 2*s*p 
    !     q = s*q
    ! enddo
end function

subroutine pythag1(a,b,p)
    use, intrinsic :: iso_fortran_env , dp=>real64
    double precision, intent(in):: a,b
    double precision, intent(out):: p
    double precision:: q ,r,s

    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    do
        r = (q/p)**2
        s= r+4
        if (s .eq. 4.0d0) exit
        s = r/s
        p = p + 2*s*p 
        q = s*q
    enddo
end subroutine

subroutine pythag2(a,b,p)
    double precision, intent(in):: a,b
    double precision, intent(out):: p
    double precision:: q ,r,s
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    do while(q>1e-10)
        r = (q/p)**2
        s = r/(4+r)
        p = p + 2*s*p 
        q = s*q
    enddo
end subroutine

subroutine pythag3(a,b,p)
    double precision, intent(in):: a,b
    double precision, intent(out):: p
    p = norm2((/a,b/))
end subroutine

subroutine pythag4(a,b,p)
    double precision, intent(in):: a,b
    double precision, intent(out):: p
    p = sqrt(a**2 + b**2)
end subroutine



program name
    implicit none
    real*8, external  :: pythag

    print *, pythag(3.0d0, 4.0d0)
    ! print*, norm2((/3.0d0,4.0d0/)),r,sqrt(3.0d0**2+4.0d0**2)
end program name

