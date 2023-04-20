program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  implicit none
  class(bmi_wrf_hydro_nwm), allocatable :: wrf_hydro
  integer :: stat
  character(len=16), pointer :: model_name
  double precision :: end_time, current_time, time_step

  allocate(wrf_hydro)
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  print * , "--- Starting ", trim(model_name), " ---"

  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(wrf_hydro%get_start_time(current_time), stat)
  call stat_check(wrf_hydro%get_end_time(end_time), stat)

  do while (current_time < end_time)
     call stat_check(wrf_hydro%update(), stat)
     call stat_check(wrf_hydro%get_current_time(current_time), stat)
  end do

  call stat_check(wrf_hydro%finalize(), stat)

  print *, "--- Finished running test driver ---"
  if (stat .ne. BMI_SUCCESS) then
     print *, "WRF-Hydro driver returned failure"
     error stop stat
  end if
end program wrf_hydro_nwm_bmi_driver
