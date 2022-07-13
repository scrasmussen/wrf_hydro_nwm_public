set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

add_pfunit_ctest (io_serial_test_1
  TEST_SOURCES io_serial_test_1.pf
  LINK_LIBRARIES hydro_orchestrator
  )

# failing
# add_pfunit_ctest (io_parallel_test_1
#   TEST_SOURCES io_parallel_test_1.pf
#   LINK_LIBRARIES hydro_orchestrator
#   MAX_PES 2)
