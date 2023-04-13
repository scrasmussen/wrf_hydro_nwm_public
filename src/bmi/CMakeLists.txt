add_library(bmi_interface bmi.f90)

add_library(bmi_wrf_hydro_nwm
  bmi_wrf_hydro_nwm_interface.f90
  bmi_wrf_hydro_nwm.f90)

target_link_libraries(bmi_wrf_hydro_nwm
  bmi_interface
  noahmp_data
  wrfhydro_lib)

add_executable(wrf_hydro_bmi.exe
  wrf_hydro_nwm_bmi_driver.f90)

target_link_libraries(bmi_wrf_hydro_nwm
  wrfhydro_lib
  module_NoahMP_hrldas)

target_link_libraries(wrf_hydro_bmi.exe
  bmi_wrf_hydro_nwm)

# --- add ctests ---
add_test(NAME wrf_hydro_bmi_test
  COMMAND wrf_hydro_bmi.exe
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
