! comparision between fourn subroutine from Numerical receipies and FFTW package
! Will forward transform the data from `fort.75` file with a length of 65536
! `fort.75` is taken from an real analysis result, in this example this can be regarded as some random data

program test
    implicit none
    integer, parameter :: fftw_forward=-1
    integer, parameter :: fftw_measure=0

    integer, parameter :: n=65536
    complex(kind=8) :: inp(n), out(n)
    integer(kind=8) :: plan
    real(kind=8) :: sn, urt(2*n)
    integer :: i, ii


    sn = 1.0d0/sqrt(real(n,kind=8))  ! normalization constant

    ! try with FFTW package 

    call dfftw_plan_dft_1d(plan,n,inp,out,fftw_forward,fftw_measure)  

    inp=0.0d0

    do i=1,9070
        read(75,'(i4, 2f20.16)') ii, inp(i)
    enddo


    ! forward transform
    call dfftw_execute_dft(plan, inp, out)

    do i =1,n ! result of FFTW is written in `fort.`101`
        write(101,'(i4, 2f20.16)') i, out(i)
    enddo

    !---------------------------------------------------------------
    ! try with fourn subroutine
    rewind(75) ! rewind the input file to read for fourn approach
    do i=1,9070
        read(75,'(i4, 2f20.16)') ii, urt(2*i-1), urt(2*i)
    enddo

    ! ISIGN is -1 as provided in this call
    call fourn(urt,[n],1,-1)

    do i=1,n ! result of fourn approach is written in `fort.`201`
        write(201,'(i4, 2f20.16)')i, urt(2*i-1), urt(2*i)
    enddo

!Result:
! `fort.101`, and `fort.201` have same result

!Conclusion:
!    We have used `fftw_forward` in FFTW approach and ISIGN=-1 in `fourn` approach,
!    which is according to the manual is inverse fourier transform (Read the header of fourn subroutine, line 75-76)
!    Now, check the FFTW definition > http://www.fftw.org/fftw3_doc/The-1d-Discrete-Fourier-Transform-_0028DFT_0029.html
!    and check the `fourn` definition line 125
!    So, its clear that `FFTW` and `fourn` uses opposite definition to define the Fourier and inverse transformation
!    If you are using forward transform in `FFTW` you have to use inverse transform with `fourn` subroutine
end program test






SUBROUTINE FOURN(DATA,NN,NDIM,ISIGN)
!     c
! c  Replaces DATA by its NDIM-dimensional discrete Fourier transform, 
! c  if ISIGN is input as 1. NN is an integer array of length NDIM, 
! c  containing the lengths of each dimension (number of complex values), 
! c  which must all be powers of 2.  DATA is a real array of length twice 
! c  the product of these lengths, in which the data are stored as in a 
! c  multidimensional complex Fortran array.  If ISIGN is input as -1, 
! c  DATA is replaced by its inverse transform times the product of the
! c  lengths of all dimensions.  From Press, W.H., Flannery, B.P., 
! c  Teukolsky, S.A., and Vetterling, W.T., 1986, Numerical Recipes, 
! c  Cambridge Univ. Press, p. 451-453.
! c
    INTEGER ISIGN,NDIM,NN(NDIM)
! C      REAL DATA(*)
    DOUBLE PRECISION DATA(*)
    INTEGER I1,I2,I2REV,I3,I3REV,IBIT,IDIM,IFP1,IFP2,IP1,IP2,IP3,K1,K2,N,NPREV,NREM,NTOT
! C      REAL TEMPI,TEMPR
    DOUBLE PRECISION TEMPI,TEMPR
    DOUBLE PRECISION THETA,WI,WPI,WPR,WR,WTEMP
    NTOT=1
    DO IDIM=1,NDIM
       NTOT=NTOT*NN(IDIM)
    ENDDO
    NPREV=1
    DO IDIM=1,NDIM
       N=NN(IDIM)
       NREM=NTOT/(N*NPREV)
       IP1=2*NPREV
       IP2=IP1*N
       IP3=IP2*NREM
       I2REV=1
       DO I2=1,IP2,IP1
          IF (I2.LT.I2REV) THEN
             DO I1=I2,I2+IP1-2,2
                DO I3=I1,IP3,IP2
                   I3REV=I2REV+I3-I2
                   TEMPR=DATA(I3)
                   TEMPI=DATA(I3+1)
                   DATA(I3)=DATA(I3REV)
                   DATA(I3+1)=DATA(I3REV+1)
                   DATA(I3REV)=TEMPR
                   DATA(I3REV+1)=TEMPI
                ENDDO
             ENDDO
          ENDIF
          IBIT=IP2/2
 1        IF ((IBIT.GE.IP1).AND.(I2REV.GT.IBIT)) THEN
             I2REV=I2REV-IBIT
             IBIT=IBIT/2
             GOTO 1
          ENDIF
          I2REV=I2REV+IBIT
       ENDDO
       IFP1=IP1
 2     IF (IFP1.LT.IP2) THEN
          IFP2=2*IFP1
          THETA=ISIGN*6.28318530717959D0/(IFP2/IP1)
          WPR=-2.0D0*SIN(0.5D0*THETA)**2
          WPI=SIN(THETA)
          WR=1.0D0
          WI=0.0D0
          DO I3=1,IFP1,IP1
             DO I1=I3,I3+IP1-2,2
                DO I2=I1,IP3,IFP2
                   K1=I2
                   K2=K1+IFP1
                   TEMPR=SNGL(WR)*DATA(K2)-SNGL(WI)*DATA(K2+1)
                   TEMPI=SNGL(WR)*DATA(K2+1)+SNGL(WI)*DATA(K2)
                   DATA(K2)=DATA(K1)-TEMPR
                   DATA(K2+1)=DATA(K1+1)-TEMPI
                   DATA(K1)=DATA(K1)+TEMPR
                   DATA(K1+1)=DATA(K1+1)+TEMPI
                ENDDO
             ENDDO
             WTEMP=WR
             WR=WR*WPR-WI*WPI+WR
             WI=WI*WPR+WTEMP*WPI+WI
          ENDDO
          IFP1=IFP2
          GOTO 2
       ENDIF
       NPREV=N*NPREV
    ENDDO
    RETURN
    END
