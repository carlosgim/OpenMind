module mod_scan_basis

!================================================================================!
! OpenMind - Score Basis Set Module
!================================================================================!
!
!================================================================================!

implicit none

integer :: iIOERR_OK = 0

public :: sScan_File

contains

! Scan File
! =========

subroutine sScan_File(cFILENAMEin, iSKIPLINES, iDATAFIELDS, iFILEROWS)

    character(LEN=200), intent(IN) :: cFILENAMEin
    integer, intent(IN) :: iSKIPLINES
    integer, intent(OUT)  :: iDATAFIELDS
    integer, intent(OUT)  :: iFILEROWS
    integer :: iUNITBASE = 30

    close(iUNITBASE)

    open(unit=iUNITBASE, file=cFILENAMEin, status = 'old', action = 'read')

    print *, 'Scanning file: ', cFILENAMEin

    rewind(UNIT=iUNITBASE)

    iDATAFIELDS = fiCount_Fields(iUNITBASE)

    iFILEROWS = fiCount_Rows(iUNITBASE)

    print *, 'FILE = ', cFILENAMEin
    print *, 'Fields = ', iDATAFIELDS
    print *, 'Rows   = ', iFileRows

    close(iUNITBASE)
   
end subroutine sScan_File

!--------------------------------------------------------------------------------
! Functions for work in field
! ****************************
!--------------------------------------------------------------------------------

 integer function fiCount_Fields(iUNITNUMBER)

    !count the number of fields by counting the delimiters
    integer, intent(IN)  :: iUNITNUMBER

    character(LEN = 20000):: miFIRSTLINE

    read(UNIT=iUNITNUMBER, FMT='(A)') miFIRSTLINE

    rewind(UNIT=iUNITNUMBER)

    fiCount_Fields= fiCountF(',',TRIM(miFIRSTLINE)) + 1

end function fiCount_Fields

integer function fiCountF(cLETTER, cSTRING)

! Count the number of occurrences of LETTER in STRING
    character(1), intent(IN) :: cLETTER
    character(*), intent(IN) :: cSTRING
    integer :: I
    fiCountF = 0
    do I = 1, LEN(cSTRING)
        if (cSTRING(I:I) == cLETTER) fiCountF = fiCountF + 1
    end do

end function fiCountF

integer function fiCount_Rows(iUNITNUMBER)

!count the number of rows in the file
    integer, intent(IN)  :: iUNITNUMBER
    character(LEN = 3)  :: cALINE
    rewind(UNIT=iUNITNUMBER)
    fiCount_Rows= 0
    do
       read(UNIT=iUNITNUMBER, FMT='(A)',END=100) cALINE
        if (TRIM(cALINE) .NE. '') THEN
           fiCount_Rows= fiCount_Rows + 1
        else
           exit
        endif
    end do
    100 continue
    rewind(UNIT=iUNITNUMBER)

end function fiCount_Rows

end module mod_scan_basis
