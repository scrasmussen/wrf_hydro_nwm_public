set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

add_pfunit_ctest (version_test_1
  TEST_SOURCES version_test_1.pf
  LINK_LIBRARIES hydro_utils
  )

add_pfunit_ctest (hydro_stop_test_1
  TEST_SOURCES hydro_stop_test_1.pf
  LINK_LIBRARIES hydro_utils hydro_mpp
  )
