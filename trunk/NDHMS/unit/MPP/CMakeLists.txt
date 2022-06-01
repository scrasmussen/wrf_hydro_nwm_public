set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

# add_pfunit_ctest (mpp_land_test_1
#   TEST_SOURCES mpp_land_init_test_1.pf
#   LINK_LIBRARIES hydro_mpp)

add_pfunit_ctest (mpp_land_init_test_2
  TEST_SOURCES mpp_land_init_test_2.pf
  LINK_LIBRARIES hydro_mpp
  MAX_PES 2)

# add_pfunit_ctest (mpp_land_test_bug_1
#   TEST_SOURCES mpp_land_init_bug_1.pf
#   LINK_LIBRARIES hydro_mpp)
