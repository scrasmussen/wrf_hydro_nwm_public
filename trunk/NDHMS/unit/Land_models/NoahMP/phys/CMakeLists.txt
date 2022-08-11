set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

# Add back in once PR644 is in this branch
# add_pfunit_ctest (sstep_negative_soil_moisture_test
#   TEST_SOURCES sstep_negative_soil_moisture_test.pf
#   LINK_LIBRARIES noahmp_phys noahmp_util
#   )
