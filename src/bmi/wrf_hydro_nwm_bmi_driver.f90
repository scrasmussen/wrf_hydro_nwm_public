program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm
  ! use bmi_wrf_hydro_nwm_smod!, only: bmi_wrf_hydro_nwm
  implicit none
  class(bmi_wrf_hydro_nwm), allocatable :: wrf_hydro

  ! allocate(wrf_hydro) ! more procedures need to be defined first

  print *, "--- Finished running test driver ---"
end program wrf_hydro_nwm_bmi_driver
