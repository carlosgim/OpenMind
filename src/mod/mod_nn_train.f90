module mod_nn_train

use mod_setup


!================================================================================!
! OpenMind
!================================================================================!
!
!
!================================================================================!

implicit none

public :: sTrain_Net

integer, parameter :: iUNITDEBUG = 101

contains

subroutine sTrain_Net(iIOERR_OK, giEPOCHS, giREDISPLAY)

    integer, intent(in) :: giREDISPLAY
    integer, intent(in) :: giEPOCHS
    integer, intent(in) :: iIOERR_OK
    integer :: i, j, k, iPAT_NUM 
    real :: rOUTPUT_THIS_PAT, rOUTPRED, rDelta
    real :: rRAND, grEtaB

!   =============================================================================================
!   ---------------------------------------------------------------------------------------------
    if (glDEBUG) then

        open(unit=iUNITDEBUG, file='OpenMind_Debug.out')

        write(iUNITDEBUG,*) " *******************Debug File************************************"

    end if

!   ---------------------------------------------------------------------------------------------
!   =============================================================================================

    call sDisplay_Headers

    grEtaB = grEtaA * 0.1D0

!   ===============================================================================================
!   -----------------------------------------------------------------------------------------------

                if (glDEBUG) then 

                    write(iUNITDEBUG,*) "garWIH(:,1)"
                    write(iUNITDEBUG,*), gatrWIH(:,1)
                    write(iUNITDEBUG,*) "garWIH(:,2)"
                    write(iUNITDEBUG,*), gatrWIH(:,2)

                end if

!   -----------------------------------------------------------------------------------------------
!   ===============================================================================================

    Loop_Epochs: do j = 1, giEPOCHS

        Loop_Pats: do i = 1, giTRAINPATS  

            call RANDOM_NUMBER(rRAND)

            iPAT_NUM = NINT(rRAND * (giTRAINPATS - 1)) + 1

            ! set the data to this pattern
            garInputs_this_pattern(:) = garTrainingInputs(iPAT_NUM,:)

            rOUTPUT_THIS_PAT = garTrainingOutputs(iPAT_NUM)

!       Feedfoward ------------------------------------------------------------------------------

            Loop_Layer_feed: do k = 1, giLayer

                gatrWIH_dummy(:,1) = garWIH(:,1)

!   ===============================================================================================
!   -----------------------------------------------------------------------------------------------

                if (glDEBUG) then 

                    write(iUNITDEBUG,*) "garWIH"
                    write(iUNITDEBUG,*), garWIH(:,1)

                end if
!   -----------------------------------------------------------------------------------------------
!   ===============================================================================================


                if ( k == 1) then

                    gatrZHO(:,1) = 1.0D0 / ( 1.0D0 + &
                        exp(-MATMUL(TRANSPOSE(gatrWIH_dummy), garInputs_this_pattern(:))))

                    !write(iUNITDEBUG,*) gatrZHO(:,1)

!   ===============================================================================================
!   -----------------------------------------------------------------------------------------------
                if (glDEBUG) then 

                    write(iUNITDEBUG,*) "gartrZHO"
                    write(iUNITDEBUG,*), gatrZHO(:,1)

                end if
!   -----------------------------------------------------------------------------------------------
!   ===============================================================================================

                end if

                !gatrZHO_dummy(:) = gatrZHO(:, k-1)

                !gatrWIH_dummy(:, 1) = gatrWIH(:, k)

                !gatrZHO(:, k) = 1.0D0 / ( 1.0D0 + exp(-MATMUL(TRANSPOSE(gatrWIH_dummy), gatrZHO_dummy(:))))

            end do Loop_Layer_feed

!       Backpropagation ---------------------------------------------------------------------------

            Loop_Layer_back: do k = 1, giLayer

                if ( k == giLayer) then

                    gatrWIH_dummy(:,1) = garWIH(:,1)

                    ! calculate the current error
                    rDelta = (SUM(garWHO*gatrZHO(:,1)) - rOUTPUT_THIS_PAT)

                    ! change weight hidden - output
                    garWHO = garWHO - grEtaB * gatrZHO(:,1) * rDelta

                    ! change weight input - hidden 
                    garTemp2(:,1) = garTrainingInputs(iPAT_NUM,:)

                    garTemp1(1,:) = (1.0D0 / (1.0D0 + exp(- gatrZHO(:,1)))) * & 
                        (1.0D0 - 1.0D0 / (1.0D0 + exp(- gatrZHO(:,1)))) * rDelta * garWHO

                    garWIH = garWIH - grEtaA * MATMUL(garTemp2, garTemp1) 

                end if

            end do Loop_Layer_back

        end do Loop_Pats

        grRMSE = frCalculate_Error  &
        (garTrainingInputs,garTrainingOutputs, garWIH, garWHO)

        !keep the new weights if an improvement has been made

        call sKeep_Best_Weights(J)

        call sDisplay_Progress(J, giEPOCHS, giREDISPLAY)

    end do Loop_Epochs

    call sDISPLAY_ERRORS


    if (glDEBUG) then
        
        close(iUNITDEBUG)

    end if

end subroutine sTrain_Net

!------------------
! Display
!------------------

subroutine sDisplay_Errors

    print 100, frCalculate_Error &
    (garTrainingInputs,garTrainingOutputs, garWIH, garWHO)

    if (giTESTPATS > 0) then
        print 110, frCalculate_Error &
        (garTestingInputs,garTestingOutputs,garWIH,garWHO)
    end if

    !if (giVALIDPATS > 0) then
        !print 120, frCalculate_Error &
        !(garValidationInputs,garValidationOutputs,garWIH,garWHO)
   ! end if

100 format('TRAIN ERROR =', 1X, F10.7)
110 format('TEST  ERROR =', 1X, F10.7)
120 format('VAL   ERROR =', 1X, F10.7)

end subroutine sDisplay_Errors

subroutine sDisplay_Headers

    if (giTESTPATS > 0) then
        print *,'epochs   TRAIN_error   TEST_error'
    else
        print *,'epochs   TRAIN_error'
    end if

end subroutine sDisplay_Headers

subroutine sDisplay_Progress(iEpch, giEPOCHS,giREDISPLAY)

    integer, intent(IN) :: iEpch
    integer, intent(IN) :: giEPOCHS
    integer, intent(IN) :: giREDISPLAY

    if ( (MODULO(iEpch,giREDISPLAY)==0) .OR. (iEpch==giEPOCHS) .OR. (iEpch==1) ) then
        if (giTESTPATS > 0) then
            grRMSETEST = frCalculate_Error &
            (garTestingInputs,garTestingOutputs,garWIH, garWHO)
            PRINT 100,iEpch,grRMSEBEST,grRMSETEST
        else
            PRINT 110,iEpch,grRMSEBEST
        end if
    end if

100 format(I5,4X,F10.7,4X,F10.7)
110 format(I5,4X,F10.7)

end subroutine sDisplay_Progress


real function frCalc_Err_This_Pat(arINPS_TP,rOUTPUT_TP,arWIHL,arWHOL)
! calculate the error on a specific pattern

    REAL,   DIMENSION(:),   INTENT(IN)  :: arINPS_TP
    REAL,   DIMENSION(:,:), INTENT(IN)  :: arWIHL
    REAL,   DIMENSION(:),   INTENT(IN)  :: arWHOL
    REAL,   INTENT(IN)  :: rOUTPUT_TP
    REAL :: rOUTPREDL

    garZout = 1.0D0 / ( 1.0D0 + exp(-MATMUL(TRANSPOSE(arWIHL),arINPS_TP)))

    ! Assing to the last column a 1
    garZout(UBOUND(garZout,1)) = 1

    rOUTPREDL=SUM(arWHOL*garZout)

    frCalc_Err_This_Pat = (rOUTPREDL - rOUTPUT_TP)

END FUNCTION frCalc_Err_This_Pat

REAL FUNCTION frCalculate_Error(arINPS,arOUT,arWIHL, arWHOL)
! calculate the overall error

    REAL, DIMENSION(:,:),   INTENT(IN)    :: arINPS
    REAL, DIMENSION(:), INTENT(IN)        :: arOUT
    REAL, DIMENSION(:,:),   INTENT(INOUT) :: arWIHL
    REAL, DIMENSION(:), INTENT(INOUT)     :: arWHOL   

    REAL, DIMENSION(LBOUND(arINPS,2):UBOUND(arINPS,2)) :: arINPUTS_THIS_PAT
    REAL :: rSQERROR, rOUTPUT_THIS_PAT, rDelta

    INTEGER :: I, iLOWER, iUPPER

    iLOWER = LBOUND(arINPS,1)
    iUPPER = UBOUND(arINPS,1)

    ! in this case the fitness function is the squared errors

    rSQERROR=0.0        

    DO I=iLOWER,iUPPER
        rOUTPUT_THIS_PAT = arOUT(I)
        arINPUTS_THIS_PAT(:)= arINPS(I,:)
        rDelta= frCalc_Err_This_Pat &
        (arINPUTS_THIS_PAT,rOUTPUT_THIS_PAT,arWIHL,arWHOL)
        rSQERROR=rSQERROR+(rDelta**2.0D0)
    ENDDO

    ! root of the mean squared error
    frCalculate_Error=SQRT(rSQERROR/(iUPPER-iLOWER+1))  

END FUNCTION frCalculate_Error

subroutine sKeep_Best_Weights(iEpch)
! if the overall error has improved then keep the new weights

    integer, intent(IN) :: iEpch

    !this will be on the first epoch
    if (iEpch .EQ. 1) then
        grRMSEBEST = grRMSE
    end if

    if (grRMSE < grRMSEBEST) then
        garWIH_Best = garWIH
        garWHO_Best = garWHO
        grRMSEBEST = grRMSE
    else
        garWIH = garWIH_Best
        garWHO = garWHO_Best
    end if

end subroutine sKEEP_BEST_WEIGHTS

end module mod_nn_train
