program tensor

implicit none

integer :: giINPPB      !INPputs Plus Bias
integer :: giNHS        !Number Hidden Start
integer :: giNHF
integer :: giHIDDDEN
integer :: i,j, k
real, dimension(:,:,:), allocatable :: garWIH
real, dimension(2,2) :: arM1, arM2, arRES

allocate(garWIH(2, 2, 2))

garWIH(1,1,1) = 1
garWIH(1,2,1) = 2
garWIH(2,1,1) = 3
garWIH(2,2,1) = 4

garWIH(1,1,2) = 5
garWIH(1,2,2) = 6
garWIH(2,1,2) = 7
garWIH(2,2,2) = 8

! Tensor

do k = 1, 2

        do i = 1, 2

            print *, (garWIH(i, j, k), j= 1, 2), k

    end do

    print *, "---"

end do

print *, "Tensor to Matrix 1"

! Tensor to Matrix 1
do i = 1, 2

    do j = 1, 2

        arM1(i,j) = garWIH(i, j, 1)

    end do

enddo



do i = 1, 2

    print *, (arM1(i,j), j= 1, 2)

end do

! Tensor to Matrix 2

do i = 1, 2

    do j = 1, 2

        arM2(i,j) = garWIH(i, j, 2)

    end do

enddo

print *, "Tensor to Matrix 2"
do i = 1, 2

    print *, (arM1(i,j), j=1,2)

end do


arRES = 0

arRES = MATMUL(TRANSPOSE(arM1),arM2)

do i = 1, 2

    print *, (arRES(i,j), j=1,2)

end do

deallocate(garWIH)

end program tensor

