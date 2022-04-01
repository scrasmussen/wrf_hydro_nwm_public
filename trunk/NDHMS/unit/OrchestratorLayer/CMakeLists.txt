set (CMAKE_Fortran_FLAGS "-lgcov  -cpp -O0 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")

add_pfunit_ctest (config_test_1
  TEST_SOURCES config_test_1.pf
  LINK_LIBRARIES hydro_orchestrator
  )

add_pfunit_ctest (io_manager_test_1
  TEST_SOURCES io_manager_test_1.pf
  LINK_LIBRARIES hydro_orchestrator
  )

add_pfunit_ctest (io_manager_test_2
  TEST_SOURCES io_manager_test_2.pf
  LINK_LIBRARIES hydro_orchestrator
  )
