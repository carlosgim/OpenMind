cmake_minimum_required(VERSION 2.8)

enable_language(Fortran)
project(FortranCMakeSample)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

add_subdirectory(src)
