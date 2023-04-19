program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm
  implicit none
  class(bmi_wrf_hydro_nwm), allocatable :: wrf_hydro
  character(len=16), pointer :: model_name
  double precision :: end_time, current_time, time_step

  allocate(wrf_hydro)
  call err_check(wrf_hydro%get_component_name(model_name))
  print * , "--- Starting ", trim(model_name), " ---"

  call err_check(wrf_hydro%initialize("no config file right now"))
  call err_check(wrf_hydro%get_start_time(current_time))
  call err_check(wrf_hydro%get_end_time(end_time))

  do while (current_time < end_time)
     call err_check(wrf_hydro%update())
     call err_check(wrf_hydro%get_current_time(current_time))
  end do

  call err_check(wrf_hydro%finalize())

  print *, "--- Finished running test driver ---"


contains
  subroutine err_check(err)
    use bmi_wrf_hydro_nwm_mod, only: BMI_SUCCESS
    integer, intent(in) :: err
    if (err .ne. BMI_SUCCESS) then
       print *, "Stopping with error code", err
       stop "ERROR"
    end if
  end subroutine err_check
end program wrf_hydro_nwm_bmi_driver
