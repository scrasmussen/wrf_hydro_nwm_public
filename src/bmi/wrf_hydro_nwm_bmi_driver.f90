program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm
  implicit none
  class(bmi_wrf_hydro_nwm), allocatable :: wrf_hydro
  integer :: res
  print *, "--- Begin running test driver ---"

  allocate(wrf_hydro)
  res = wrf_hydro%initialize("no config file right now")

  print *, "--- Finished running test driver ---"
end program wrf_hydro_nwm_bmi_driver
