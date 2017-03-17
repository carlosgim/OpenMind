# ============================================================================
# Name        : Open Mind Code
# Author      : Carlos A. Gimenez
# Version     :
# Description : This project work correctly
# ============================================================================

.PHONY: all clean

# Change this line if you are using a different Fortran compiler

FORTRAN_COMPILER = gfortran

all: src/OpenMind.f90
	$(FORTRAN_COMPILER) -O2 -g \
		-o bin/NN_kernel.exe \
		src/*.f90

clean:
	rm -f bin/NN_kernel.exe *.mod
	

