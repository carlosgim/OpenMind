module mod_save_ann

!================================================================================!
! OpenMind
!================================================================================!
! 
!================================================================================!

implicit none

public :: sBinary_save_ANN

contains

    subroutine sBinary_save_ANN(arWIHL, arWHOL, iINPPB, iNHS, iNHF, arMaxInp, arMinInp)

        integer, intent(in) :: iINPPB, iNHS, iNHF
        real, intent(in) :: arWIHL(iINPPB,iNHS:iNHF)
        real, intent(in) :: arWHOL(iNHS:iNHF)
        real, intent(in) :: arMaxInp(iINPPB), arMinInp(iINPPB)

        open(unit=201, file= 'iINPPB_saved.bin', status = 'new', action = 'write')
        open(unit=202, file= 'arWIHL_saved.bin', status = 'new', action = 'write')
        open(unit=203, file= 'arWHOL_saved.bin', status = 'new', action = 'write')
        open(unit=204, file= 'arMaxInp_saved.bin', status = 'new', action = 'write')
        open(unit=205, file= 'arMinInp_saved.bin', status = 'new', action = 'write')


        write(201, *) iINPPB, iNHS, iNHF
        write(202, *) arWIHL
        write(203, *) arWHOL
        write(204, *) arMaxInp
        write(205, *) arMinInp

        close(201)
        close(202)
        close(203)
        close(204)
        close(205)

    end subroutine sBinary_save_ANN

    subroutine sBinary_load_ANN(arWIHL, arWHOL, iINPPB, iNHS,iNHF, arMaxInp, arMinInp)

        integer, intent(out) :: iINPPB, iNHS, iNHF
        real, allocatable :: arWIHL(:,:)
        real, allocatable :: arWHOL(:)
        real, allocatable :: arMaxInp(:), arMinInp(:)

        open(unit=201, file= 'iINPPB_saved.bin', status = 'old', action = 'read')
        open(unit=202, file= 'arWIHL_saved.bin', status = 'old', action = 'read')
        open(unit=203, file= 'arWHOL_saved.bin', status = 'old', action = 'read')
        open(unit=204, file= 'arMaxInp_saved.bin', status = 'old', action = 'read')
        open(unit=205, file= 'arMinInp_saved.bin', status = 'old', action = 'read')

        read(201,*) iINPPB, iNHS, iNHF

        allocate(arWIHL(iINPPB,iNHS:iNHF))
        allocate(arWHOL(iNHS:iNHF))
        allocate(arMaxInp(iINPPB))
        allocate(arMinInp(iINPPB))

        read(202, *) arWIHL
        read(203, *) arWHOL
        read(204, *) arMaxInp
        read(205, *) arMinInp

        close(201)
        close(202)
        close(203)
        close(204)
        close(205)

      end subroutine sBinary_load_ANN

end module mod_save_ann
