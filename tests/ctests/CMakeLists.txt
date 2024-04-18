# add Makefile target to setup test runs
# --- croton test options: {Gridded, Gridded_no_lakes, NWM, Reach, ReachLakes}
add_custom_target(croton
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh Gridded ${CMAKE_BINARY_DIR})
add_custom_target(croton-gridded
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh Gridded ${CMAKE_BINARY_DIR})
add_custom_target(croton-gridded-no-lakes
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh Gridded_no_lakes ${CMAKE_BINARY_DIR})
add_custom_target(croton-nwm
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh NWM ${CMAKE_BINARY_DIR})
add_custom_target(croton-reach
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh Reach ${CMAKE_BINARY_DIR})
add_custom_target(croton-reach-lakes
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh ReachLakes ${CMAKE_BINARY_DIR})


# --- generic ctests ---
add_executable(fortran_ctest_should_fail
  should_fail.f90)
add_test(NAME fortran_ctest_should_fail
  COMMAND fortran_ctest_should_fail)
set_tests_properties(fortran_ctest_should_fail
  PROPERTIES WILL_FAIL TRUE)

add_executable(fortran_ctest_should_not_fail
  should_not_fail.f90)
add_test(NAME fortran_ctest_should_not_fail
  COMMAND fortran_ctest_should_not_fail)


# --- bmi ctests ---
add_test(NAME wrf_hydro_bmi_test
  COMMAND wrf_hydro_bmi.exe
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)

set_tests_properties(wrf_hydro_bmi_test
  PROPERTIES DEPENDS setup_wrf_hydro_testcase)
