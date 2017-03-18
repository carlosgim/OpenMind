add_executable(OpenMind.exe OpenMind.f90)
target_link_libraries(OpenMind.exe mod)

add_subdirectory(mod)
