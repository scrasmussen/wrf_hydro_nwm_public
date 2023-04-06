add_library(bmi_interface bmi.f90)

# this is for testing purposes only
add_library(wrf_hydro_foo_model
  wrf_hydro_foo_model.f90)

add_library(bmi_wrf_hydro_nwm_mod
  bmi_wrf_hydro_nwm_interface.f90
  bmi_wrf_hydro_nwm.f90)

target_link_libraries(bmi_wrf_hydro_nwm_mod
  bmi_interface
  wrf_hydro_foo_model)

add_executable(wrf_hydro_bmi.exe
  wrf_hydro_nwm_bmi_driver.f90)

target_link_libraries(wrf_hydro_bmi.exe
  bmi_wrf_hydro_nwm_mod)
