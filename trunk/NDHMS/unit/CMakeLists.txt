add_pfunit_ctest (unit_test_example1
  TEST_SOURCES example1.pf
  LINK_LIBRARIES hydro_utils
  )

add_pfunit_ctest (unit_test_example2
  TEST_SOURCES example2.pf
  LINK_LIBRARIES hydro_utils
  )

add_subdirectory(OrchestratorLayer)
