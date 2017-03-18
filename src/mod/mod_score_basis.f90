module mod_score_basis

use mod_scan_basis, only: sScan_File

!use mod_output

!================================================================================!
! OpenMind - Score Basis Set Module
!================================================================================!
!
! This module run after the training is finished.
!
! INPUTS: Basis set for scoring
!
! OUTPUT: Basis  set scored
!
! NOTE: We need to set the mod_output
!
!================================================================================!

implicit none

public :: sScore_Basis_Set

contains

subroutine sScore_Basis_Set(arWIHL, arWHOL, iINPPB, iNHS, iNHF, arMaxInp, arMinInp,&
    cFILESCOREin, cFILENAMEOUTin)

    real, dimension(:,:), intent(in) :: arWIHL
    real, dimension(:), intent(in) :: arWHOL
    real, dimension(:) :: arMaxInp(:), arMinInp(:)
    integer, intent(in) :: iINPPB, iNHS, iNHF
    real :: rOUTPREDL
    integer :: I,K
    integer :: iLOWER, iUPPER
    real, allocatable :: arDataBasis(:,:), arDataBasisScaled(:,:)
    real, allocatable :: arHVAL(:)
    integer ::  iBASISFIELDS, iBASISFILEROWS
    real :: yhat
    character(LEN = 20000):: miFIRSTLINE
    character(LEN = 200) :: cFILESCOREin, cFILENAMEOUTin
    real :: start, finish

    ! Open the basis set for scoring
    ! More later, we need to take this routine from sub openfile

    ! Load basis set
    ! ==============

    open(UNIT=120, FILE=cFILENAMEOUTin , STATUS='NEW')

    call sScan_File(cFILESCOREin,1, iBASISFIELDS, iBASISFILEROWS)

    open(UNIT=100, FILE=cFILESCOREin, STATUS='OLD')

    rewind(100)

    allocate(arDataBasis(iBASISFILEROWS, iBASISFIELDS))
    allocate(arDataBasisScaled(iBASISFILEROWS, iBASISFIELDS))


    read(100,*) miFIRSTLINE

    do I = 1, iBASISFILEROWS-1
        read(100,*)(arDataBasis(I,K),K = 1, iBASISFIELDS)
    end do

    close(100)

    ! Scaling basis  set
    ! ===================

    do i = 1, iBASISFILEROWS
        arDataBasisScaled(i,1:iBASISFIELDS-1) = &
            ((arDataBasis(i,1:iBASISFIELDS-1) - arMinInp(1:iBASISFIELDS-1)) / &
            (arMaxInp(1:iBASISFIELDS-1) - arMinInp(1:iBASISFIELDS-1)))
    end do

    iLOWER = LBOUND(arDataBasis,1)
    iUPPER = UBOUND(arDataBasis,1)

    allocate(arHVAL(iNHS:iNHF))

    call cpu_time(start)

    do I = iLOWER, iUPPER-1

        arHVAL=1.0D0/(1.0D0+exp(-(MATMUL(TRANSPOSE(arWIHL),arDataBasisScaled(I,:)))))

        arHVAL(UBOUND(arHVAL,1)) = 1

        rOUTPREDL= ABS(SUM(arWHOL*arHVAL))

        write(120,*) I, (arDataBasis(I,K),K = 1, iBASISFIELDS-1), rOUTPREDL

    end do

    call cpu_time(finish)

    print '("Time = ",f6.3," seconds.")',finish-start

    close(120)

    print *, 'Scored!'

end subroutine sScore_Basis_Set

end module mod_score_basis
