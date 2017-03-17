module mod_setup

!================================================================================!
! Setup the environment for Star of Death III
!================================================================================!
!
!
!================================================================================!

use mod_open_input

implicit none

public :: sSetup

integer :: giIOERR_OK = 0
real, allocatable :: garDataArray(:,:)
real, allocatable :: garTrainingInputs(:,:), garTrainingOutputs(:)
real, allocatable :: garTestingInputs(:,:), garTestingOutputs(:)
real, allocatable :: garValidationInputs(:,:), garValidationOutputs(:)
real, allocatable :: garInputs_this_pattern(:)
real, allocatable :: garWIH(:,:),garWIH_Best(:,:)

real, allocatable :: garWHO(:),garWHO_Best(:)
real, allocatable :: garDelta(:,:)

real, allocatable :: garZout(:)
real, allocatable :: garTemp1(:,:),garTemp2(:,:)
real, allocatable :: garMaxInp(:),garMinInp(:)
real :: grMaxOut, grMinOut


! Tensorial
real, allocatable :: gatrWIH(:,:),gatrWIH_Best(:,:)
real, allocatable :: gatrWIH_dummy(:,:)
real, allocatable :: gatrZHO(:,:)
real, allocatable :: gatrZHO_dummy(:)

!network topolgy numbering (dependent on number of hidden neurons)
integer :: giNHS            !Number Hidden Start
integer :: giNHF            !Number Hidden Finish
integer :: giNOS            !Number Output Start

!general network numbering (independent of number of hidden neurons)
integer :: giINPPB      !INPputs Plus Bias
integer :: giINPUTS
integer :: iOUTPUTS
integer :: giNDU        !Number Data Units

!information about the source data file
!integer :: giDATAFIELDS, giFILEROWS, giSKIPLINES, giPERCTR, giPERCTE

integer :: giPATS, giTRAINPATS, giTESTPATS, giVALIDPATS
real    :: grRMSE, grRMSEBEST, grRMSETEST
integer :: giDATAFIELDS, giFILEROWS


logical :: giDEBUG

contains

subroutine sSetup

implicit none

    call sRead_input
    call sScan_File(cFILENAME, giSKIPLINES, giDATAFIELDS, giFILEROWS)
    call Random_Seed
    call sSet_Data_Constants(giPATS, giINPUTS, iOUTPUTS, giNDU, giINPPB, &
     giFILEROWS, giSKIPLINES, giDATAFIELDS)
    call sGet_Set_Sizes(giTRAINPATS, giTESTPATS, giVALIDPATS, giPATS, giIOERR_OK, giPERCTR, giPERCTE)
    call sAllocate_Data_Arrays
    call sRead_Data(cFILENAME ,giPATS, giNDU, garDataArray, giSKIPLINES, giIOERR_OK)
    ! Processing data - Puede ir en otro modulo
    call sCreate_Training_Data
    call sScale_Data
    call sSet_Weight_Constants(giIOERR_OK)
    call sAllocate_Weight_Arrays
    call sInitiate_Weights(garWIH, garWHO, garWIH_Best, garWHO_Best)
    call sInit_TWeights(gatrWIH,giLayer)

return
end subroutine sSetup

! ----------------------------------------------------------------------------------!

subroutine sSet_Data_Constants &
    (iNPATS,iINPUTS,iNOUTPUTS,iNDU,iINPPB,giFILEROWS,giSKIPLINES,giDATAFIELDS)

    integer, intent (IN)    :: giFILEROWS
    integer, intent (IN)    :: giSKIPLINES
    integer, intent (IN)    :: giDATAFIELDS

    integer, intent (OUT)   :: iNPATS
    integer, intent (OUT)   :: iINPUTS
    integer, intent (OUT)   :: iNOUTPUTS
    integer, intent (OUT)   :: iNDU
    integer, intent (OUT)   :: iINPPB

    iNPATS = giFILEROWS - giSKIPLINES
    iINPUTS = giDATAFIELDS - 1
    iNOUTPUTS = 1         ! number of outputs (fixed)
    iNDU = iINPUTS + iNOUTPUTS      !Number Data Units
    iINPPB = iINPUTS + 1        !INPut Plus Bias

end subroutine sSet_Data_Constants

subroutine sGet_Set_Sizes(iTRAIN,iTEST,iVALID,iTOTAL,iIOERR_OK,giPERCTR,giPERCTE)

    integer, intent(IN) :: giPERCTR, giPERCTE
    integer, intent(IN) :: iIOERR_OK, iTOTAL!, iPERCTR, iPERCTE
    integer, intent(OUT) :: iTRAIN, iTEST, iVALID

    integer :: I

    iTRAIN = iTOTAL
    iTEST = 0
    iVALID = 0

    DO

        IF (giPERCTR >= 100) THEN
            PRINT *, 'OK - all the data will be used for training...'
        ENDIF

        IF (giPERCTR<33) THEN
            PRINT *, 'We will use 33%...'
            !iPERCTR = 33
        ENDIF

        iTRAIN = iTOTAL * giPERCTR * 0.01

        EXIT

    ENDDO


        IF (giPERCTR < 100) THEN

     DO


        IF (giPERCTE>100-giPERCTR) CYCLE
        IF (giPERCTE<=0) CYCLE

        iTEST = iTOTAL * giPERCTE * 0.01
        IF (iTEST<1) iTEST = 1

        EXIT

     ENDDO

        ENDIF

    PRINT * , ' =============================='
    print *,  ' Setting set size parameters...'
    PRINT * , ' ------------------------------'
    iVALID = iTOTAL - iTRAIN - iTEST
    
    PRINT * , ' '
    PRINT 100 , iTOTAL
    PRINT 110 , giPERCTR, iTRAIN
    PRINT 120 , giPERCTE, iTEST
    PRINT 130 ,100-giPERCTR-giPERCTE,iVALID
    PRINT * , ' =============================='

100 FORMAT (' Total patterns = ',I10)
110 FORMAT (' Train =',1X,I3,'%',I7,1X,'Patterns')
120 FORMAT (' Test  =',1X,I3,'%',I7,1X,'Patterns')
130 FORMAT (' Valid =',1X,I3,'%',I7,1X,'Patterns')

END subroutine sGet_Set_Sizes

!--------------------------------------------------------------------------------
! Functions for work in field
! ****************************
!--------------------------------------------------------------------------------

integer function fiGet_Header_Rows(iIOERR_OK)

    integer, intent(IN) :: iIOERR_OK
    integer :: I

    do
     write(*,'(A)',advance='no',iostat=I) 'How many header rows in the file?'
     if(I /= iIOERR_OK) EXIT
        read(*,*,iostat=I) fiGet_Header_Rows
        if(I /= iIOERR_OK) CYCLE
        if(fiGet_Header_Rows< 0) THEN
         print *, 'OK - no header row...'
         fiGet_Header_Rows= 0
        endif
     exit
    end do

end function fiGet_Header_Rows

subroutine sAllocate_Data_Arrays

    allocate(garDataArray(giPATS,giNDU)) !raw data read from file
    allocate(garTrainingInputs(giTRAINPATS,giINPPB)) !input patterns
    allocate(garTrainingOutputs(giTRAINPATS)) !output
    allocate(garTestingInputs(giTESTPATS,giINPPB)) !input patterns
    allocate(garTestingOutputs(giTESTPATS)) !output
    allocate(garValidationInputs(giVALIDPATS,giINPPB)) !input patterns
    allocate(garValidationOutputs(giVALIDPATS)) !output
    allocate(garInputs_this_pattern(giINPPB)) !pattern being presented
    allocate(garMaxInp(giINPPB))
    allocate(garMinInp(giINPPB))

end subroutine sAllocate_Data_Arrays



!--------------------------
! input file handling
!--------------------------

subroutine sRead_Data(cFILEBASE, iPATTERNS, iFIELDS, arDATAARRAY, iHEADERROWS, iIOERR_OK)

    character(LEN=200), intent(IN)  :: cFILEBASE
    integer, intent(IN)  :: iFIELDS
    integer, intent(IN)  :: iHEADERROWS
    integer, intent(IN)  :: iIOERR_OK
    integer, intent(IN)  :: iPATTERNS
    real, intent(OUT)    :: arDATAARRAY(iPATTERNS,iFIELDS)
    character(LEN = 100) :: cHEAD
    integer              :: I,K
    integer              :: mis
    integer :: iUNITNUMBER=25

    open(unit=iUNITNUMBER, file=cFILEBASE, status = 'old', action = 'read')

    if (iHEADERROWS > 0) then
        do I = 1, iHEADERROWS
            read(UNIT=iUNITNUMBER, FMT='(A)',END=100) cHEAD
        end do
        100 IF (I < iHEADERROWS) PRINT *,'Too many header rows...'
    end if

    print *, ' Reading data...'

    do I = 1, iPATTERNS
        read(UNIT=iUNITNUMBER, FMT=*,iostat=mis, END=200)(arDATAARRAY(I,K),K=1,iFIELDS)
        if(mis /= iIOERR_OK) then
          print *, 'Invalid data on line ', i + iHEADERROWS
          stop
        end if
    end do

    print *, ' Data read OK!'

        200 IF (I < iPATTERNS) print *,'Too many header rows...'

    close(iUNITNUMBER)

end subroutine sRead_Data


subroutine sCreate_Training_Data

    integer :: iTRAINCOUNT,iTESTCOUNT,iVALIDCOUNT
    integer :: I
    real :: rTRPC,rTEPC
    real :: rRAND

    rTRPC = FLOAT(giTRAINPATS) / FLOAT(giPATS)
    rTEPC = rTRPC + (FLOAT(giTESTPATS) / FLOAT(giPATS))

    iTRAINCOUNT = 0
    iTESTCOUNT = 0
    iVALIDCOUNT = 0

    print *, ' Allocating data...'

    loop_Pats: do I = 1, giPATS

        call RANDOM_NUMBER(rRAND)

        if ((rRAND <= rTRPC) .AND. (iTRAINCOUNT < giTRAINPATS)) then
            iTRAINCOUNT = iTRAINCOUNT + 1
            garTrainingInputs(iTRAINCOUNT, 1:giINPUTS) = garDataArray(I, 1:giINPUTS)
            ! We need to consider the this equality for conserve the dimension of the
            ! MATMUL. All the weigths have one bias.
            garTrainingInputs(iTRAINCOUNT, giINPPB) = 1
            garTrainingOutputs(iTRAINCOUNT) = garDataArray(I, giNDU)

        elseif ((rRAND <= rTEPC) .AND. (iTESTCOUNT < giTESTPATS)) then
            iTESTCOUNT = iTESTCOUNT + 1
            garTestingInputs(iTESTCOUNT, 1:giINPUTS) = garDataArray(I, 1:giINPUTS)
            garTestingInputs(iTESTCOUNT, giINPPB) = 1
            garTestingOutputs(iTESTCOUNT) = garDataArray(I, giNDU)

        elseif ((rRAND > rTEPC) .AND. (iVALIDCOUNT < giVALIDPATS)) then
            iVALIDCOUNT = iVALIDCOUNT + 1
            garValidationInputs(iVALIDCOUNT, 1:giINPUTS) = garDataArray(I, 1:giINPUTS)
            garValidationInputs(iVALIDCOUNT,giINPPB) = 1
            garValidationOutputs(iVALIDCOUNT) = garDataArray(I, giNDU)

        elseif (iTRAINCOUNT < giTRAINPATS) then
            iTRAINCOUNT = iTRAINCOUNT + 1
            garTrainingInputs(iTRAINCOUNT, 1:giINPUTS)=garDataArray(I, 1:giINPUTS)
            garTrainingInputs(iTRAINCOUNT, giINPPB) = 1
            garTrainingOutputs(iTRAINCOUNT) = garDataArray(I, giNDU)

        elseif (iTESTCOUNT < giTESTPATS) then
            iTESTCOUNT = iTESTCOUNT + 1
            garTestingInputs(iTESTCOUNT, 1:giINPUTS) = garDataArray(I, 1:giINPUTS)
            garTestingInputs(iTESTCOUNT, giINPPB) = 1
            garTestingOutputs(iTESTCOUNT) = garDataArray(I, giNDU)

        elseif (iVALIDCOUNT < giVALIDPATS) then
            iVALIDCOUNT = iVALIDCOUNT + 1
            garValidationInputs(iVALIDCOUNT, 1:giINPUTS) = garDataArray(I, 1:giINPUTS)
            garValidationInputs(iVALIDCOUNT, giINPPB) = 1
            garValidationOutputs(iVALIDCOUNT) = garDataArray(I, giNDU)

        endif

    end do loop_Pats

   ! deallocate(garDataArray)

    print *, ' Data allocated OK!'

end subroutine sCreate_Training_Data

subroutine sScale_Data

    integer :: i

    print *, ' Scaling data...'

    garMaxInp(:) = MAXVAL(garDataArray, 1)
    garMinInp(:) = MINVAL(garDataArray, 1)
    grMaxOut = MAXVAL(garTrainingOutputs)
    grMinOut = MINVAL(garTrainingOutputs)

    do i = 1, giTRAINPATS
        garTrainingInputs(i, 1:giINPUTS) = &
            ((garTrainingInputs(i, 1:giINPUTS) - garMinInp(1:giINPUTS)) / &
            (garMaxInp(1:giINPUTS) - garMinInp(1:giINPUTS)))
    end do

    garTrainingOutputs(:) = &
        ((garTrainingOutputs(:) - grMinOut) / (grMaxOut - grMinOut))

    if (giTESTPATS > 0) then
        do i = 1, giTESTPATS
            garTestingInputs(i,1:giINPUTS) = &
                ((garTestingInputs(i,1:giINPUTS) - garMinInp(1:giINPUTS)) / &
                (garMaxInp(1:giINPUTS) - garMinInp(1:giINPUTS)))
        end do
            garTestingOutputs(:) = &
                ((garTestingOutputs(:) - grMinOut) / (grMaxOut - grMinOut))
    end if

    print *, ' Data scaled OK!'
    print *, ''

end subroutine sScale_Data

subroutine sSet_Weight_Constants(iIOERR_OK)

    integer, intent(IN) :: iIOERR_OK

    giHIDDDEN = giHIDDDEN+1       !accounts for bias to output
    giNHS = giINPPB+1             !Number Hidden Start
    giNHF = giINPPB+giHIDDDEN     !Number Hidden Finish
    giNOS = giNHF+1               !Number Output Start

end subroutine sSet_Weight_Constants

subroutine sAllocate_Weight_Arrays

    allocate(garWIH(giINPPB,giNHS:giNHF))       !input-hidden weights
    allocate(garWIH_Best(giINPPB,giNHS:giNHF))  !best weights
    allocate(garWHO(giNHS:giNHF))               !hidden-output weights
    allocate(garWHO_Best(giNHS:giNHF))          !best weights
    allocate(garDelta(giNHS:giNHF,giLayer))
    allocate(garZout(giNHS:giNHF))              !hidden neuron outputs

    allocate(garTemp1(1,giNHS:giNHF))          !dummy matrix
    allocate(garTemp2(giINPPB,1))              !dummy matrix

    ! Tensorial
    allocate(gatrWIH(giINPPB, giLayer))       !For the moment: The max size of giHIDDDEN=giINPPB
    allocate(gatrWIH_dummy(giINPPB, giLayer))     
    allocate(gatrWIH_Best(giINPPB, giLayer))  !best weights
    allocate(gatrZHO(giNHS:giNHF,giLayer))
    allocate(gatrZHO_dummy(giNHS:giNHF))

end subroutine sAllocate_Weight_Arrays



subroutine sInitiate_Weights(arWIHL,arWHOL,arWIHBESTL,arWHOBESTL)
! generate initial random weights

    real, dimension (:,:),  intent (INOUT) :: arWIHL
    real, dimension (:,:),  intent (INOUT) :: arWIHBESTL
    real, dimension (:),    intent (INOUT) :: arWHOL
    real, dimension (:),    intent (INOUT) :: arWHOBESTL

    integer :: J, K
    real :: rRAND

    do K=LBOUND(arWIHL,2),UBOUND(arWIHL,2)
        call RANDOM_NUMBER(rRAND)
        arWHOL(K)=((rRAND-0.5)*2)*0.1
        do J=LBOUND(arWIHL,1),UBOUND(arWIHL,1)
            call RANDOM_NUMBER(rRAND)
            arWIHL(J,K)=((rRAND-0.5)*2)*0.1
        end do
    end do

    arWIHBESTL=arWIHL !record of best weights so far
    arWHOBESTL=arWHOL !record of best weights so far

end subroutine sInitiate_Weights


subroutine sInit_TWeights(arWIHL,iLayer)

    real, dimension (:,:),  intent(inout) :: arWIHL

    integer :: i, k, iLayer
    real :: rRAND

    loop_rows: do i = 1, giINPPB

        loop_column: do k = 1, iLayer

            call RANDOM_NUMBER(rRAND)

            gatrWIH(i,k)=((rRAND-0.5)*2)*0.1

        end do loop_column

    end do loop_rows

end subroutine sInit_TWeights

end module mod_setup
