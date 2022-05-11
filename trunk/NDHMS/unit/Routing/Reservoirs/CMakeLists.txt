set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

add_pfunit_ctest (all_reservoir_unit_tests
  TEST_SOURCES all_reservoir_unit_tests.pf
  LINK_LIBRARIES hydro_levelpool_tests reservoir_unit_tests
  )
