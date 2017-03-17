module mod_open_input

use mod_scan_basis, only: sScan_File
!================================================================================!
! Read Input module for OpenMind
!================================================================================!
! The porpouse of this module is get all the environment variables
! and initial setting for neural network
!================================================================================!

implicit none

    integer, parameter :: dp = kind(0.d0)
    integer :: fiGet_Hidden_Neurons, giEPOCHS, giPERCTR
    integer :: giPERCTE,  giREDISPLAY, iIOERR_OK, giSKIPLINES
    integer :: iUNITKERN = 10
    integer :: giHIDDDEN
    integer :: giLayer
    real(dp) :: grEtaA
    character(LEN=200) :: cFILENAME
    character(LEN=200) :: cFILENAMEOUT
    character(LEN=200) :: cFILESCORE
    character(LEN = 20000) :: miFIRSTLINE
    character(LEN = 3) :: cALINE
    character*80 :: cDUMMY1
    integer :: giKIND
    logical :: glDEBUG

public :: sRead_input, fiGet_Hidden_Neurons, giEPOCHS, giPERCTR, giPERCTE, giSKIPLINES, giREDISPLAY, &
          iIOERR_OK, grEtaA, cFILENAME, giKIND, cFILENAMEOUT, cFILESCORE, glDEBUG

contains

! OPEN & READ "KERNEL INPUT"
! ==========================

subroutine sRead_input

    open(unit=iUNITKERN, file='Kernel.inp', status = 'old', action = 'read')
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giKIND
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) cFILENAME
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) cFILESCORE
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giHIDDDEN
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giLayer
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giEPOCHS
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) grEtaA
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giPERCTR
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giPERCTE
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giSKIPLINES
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) giREDISPLAY
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) cFILENAMEOUT
    read(iUNITKERN,*) cDUMMY1
    read(iUNITKERN,*) glDEBUG

    close(iUNITKERN)

end subroutine sRead_input

end module mod_open_input



