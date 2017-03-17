module mod_score_bot

!use mod_output

!================================================================================!
! OpenMind - Score Basis Set Module calling from Bot
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

public :: sScore_Bot_Set

contains

subroutine sScore_Bot_Set(arWIHL, arWHOL, iINPPB, iNHS, iNHF, arMaxInp, arMinInp, cFILESCOREin, cFILENAMEOUTin)

    real, dimension(:,:), intent(in) :: arWIHL
    real, dimension(:), intent(in) :: arWHOL
    real, dimension(:) :: arMaxInp(:), arMinInp(:)
    integer, intent(in) :: iINPPB, iNHS, iNHF
    real :: rOUTPREDL
    integer :: I, K
    integer :: iLOWER, iUPPER
    real, allocatable :: arDataBasis(:), arDataBasisScaled(:)
    real, allocatable :: arHVAL(:)
    integer ::  iBASISFIELDS
    real :: yhat
    character(LEN=200) :: cFILESCOREin, cFILENAMEOUTin

    ! Load basis set
    ! ==============

    iBASISFIELDS = 40

    open(UNIT=124, FILE=cFILESCOREin, STATUS='OLD')
    open(UNIT=120, FILE=cFILENAMEOUTin , STATUS='NEW')

    allocate(arDataBasis(iBASISFIELDS))
    allocate(arDataBasisScaled(iBASISFIELDS))

    read(124,*) (arDataBasis(K),K = 1, iBASISFIELDS)

    close(124)

    ! Scaling set
    ! ===========

    arDataBasisScaled(1:iBASISFIELDS) = &
        ((arDataBasis(1:iBASISFIELDS) - arMinInp(1:iBASISFIELDS)) / &
        (arMaxInp(1:iBASISFIELDS) - arMinInp(1:iBASISFIELDS)))

    allocate(arHVAL(iNHS:iNHF))

    arHVAL=1.0D0/(1.0D0+exp(-(MATMUL(TRANSPOSE(arWIHL), arDataBasisScaled))))

    arHVAL(UBOUND(arHVAL,1)) = 1

    rOUTPREDL=ABS(SUM(arWHOL*arHVAL))

    write(120,*) rOUTPREDL

    close(120)

    print *, 'Scored!'

end subroutine sScore_Bot_Set

end module mod_score_bot
