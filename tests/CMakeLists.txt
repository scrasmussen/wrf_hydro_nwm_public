# --- add ctests ---
add_test(NAME setup_wrf_hydro_testcase
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh ${CMAKE_BINARY_DIR})

add_test(NAME wrf_hydro_bmi_test
  COMMAND wrf_hydro_bmi.exe
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)

set_tests_properties(wrf_hydro_bmi_test
  PROPERTIES DEPENDS setup_wrf_hydro_testcase)
