set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

add_pfunit_ctest (unit_test_example1
  TEST_SOURCES example1.pf
  LINK_LIBRARIES hydro_utils
  )

add_pfunit_ctest (unit_test_example2
  TEST_SOURCES example2.pf
  LINK_LIBRARIES hydro_utils
  )

add_subdirectory(IO)
add_subdirectory(OrchestratorLayer)
# add_subdirectory(nudging)
add_subdirectory(utils)
add_subdirectory(Routing)
add_subdirectory(MPP)
