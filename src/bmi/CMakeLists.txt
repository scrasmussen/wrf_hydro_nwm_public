add_library(bmi_interface bmi.f90)

add_library(bmi_wrf_hydro_nwm
  bmi_wrf_hydro_nwm_interface.f90
  bmi_wrf_hydro_nwm.f90)

target_link_libraries(bmi_wrf_hydro_nwm
  bmi_interface)
