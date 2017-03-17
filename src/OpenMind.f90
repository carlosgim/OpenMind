! ============================================================================
! Name        : NN_OpenMind.f90
! Author      : Carlos A. Gimenez
! Version     :
! Description : 
! ============================================================================

program nn_main
use mod_open_input, only: sRead_input
use mod_setup
use mod_nn_train, only: sTrain_Net
use mod_score_basis, only: sScore_Basis_Set
use mod_save_ann
use mod_score_bot


implicit none

integer :: iINPPB_bin, iNHS_bin, iNHF_bin
real, allocatable :: arWIHL(:,:)
real, allocatable :: arWHOL(:)
real, allocatable :: arMaxInp_bin(:)
real, allocatable :: arMinInp_bin(:)

! a = array
! t = tensor
! i = integer
! r = real
! l = logical
! c = character
! f = function
! s = subroutine
! g = global variable
! m = module

call sRead_input

if (giKIND == 0) then

    print *, 'Running train and save neural network...'

    call sSetup

    call sTrain_Net(giIOERR_OK, giEPOCHS, giREDISPLAY)

    call sBinary_save_ANN(garWIH_Best, garWHO_Best, giINPPB, giNHS, giNHF, garMaxInp, garMinInp)

else if (giKIND == 1) then

    print *, 'Running load neural network and scoring basis...'

    call sBinary_load_ANN(arWIHL, arWHOL, iINPPB_bin, iNHS_bin, iNHF_bin, arMaxInp_bin, arMinInp_bin)

    call sScore_Basis_Set(arWIHL,arWHOL, iINPPB_bin, iNHS_bin, iNHF_bin, arMaxInp_bin, &
        arMinInp_bin, cFILESCORE, cFILENAMEOUT)

else

    print *, 'Running load neural network and scoring bot...'

    call sBinary_load_ANN(arWIHL, arWHOL, iINPPB_bin, iNHS_bin, iNHF_bin, arMaxInp_bin, arMinInp_bin)

    call sScore_Bot_Set(arWIHL,arWHOL, iINPPB_bin, iNHS_bin, iNHF_bin, arMaxInp_bin, arMinInp_bin, &
        cFILESCORE, cFILENAMEOUT)

end if

stop

end program nn_main


