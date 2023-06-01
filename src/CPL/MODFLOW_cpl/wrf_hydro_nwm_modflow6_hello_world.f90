program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_modflow_mod, only : modflow6, bmi_modflow, BMI_LENCOMPONENTNAME
  use iso_c_binding, only : c_char, C_NULL_CHAR
  implicit none

  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  type(bmi_modflow) :: modflow

  character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
  character(len=256), pointer :: mf_model_name

  character(len=BMI_MAX_VAR_NAME) :: time_unit
  character(len=BMI_MAX_VAR_NAME) :: mf_time_unit
  double precision :: end_time, current_time, time_step, mf_time_step
  integer :: res, stat, i
  integer, allocatable :: end(:), var_data(:), grid_shape(:)
  integer, allocatable :: ind_data(:), indices_data(:), indices(:)
  integer, pointer ::  ptr_data(:)
  real, allocatable :: foo_data(:)
  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  modflow = modflow6()
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  call stat_check(modflow%get_component_name(mf_model_name), stat)

  print * , "--- Starting ", trim(model_name), " and ", trim(mf_model_name), " ---"

  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(modflow%initialize(""), stat)

  call stat_check(wrf_hydro%get_start_time(current_time), stat)
  call stat_check(wrf_hydro%get_end_time(end_time), stat)

  call stat_check(wrf_hydro%get_time_step(time_step), stat)
  call stat_check(modflow%get_time_step(mf_time_step), stat)

  call stat_check(wrf_hydro%get_time_units(time_unit), stat)
  call stat_check(modflow%get_time_units(mf_time_unit), stat)
  print *, "end time:", end_time, "current time:", current_time

  do while (current_time < end_time)
     ! update models
     call stat_check(wrf_hydro%update(), stat)
     if (mod(current_time, mf_time_step) == 0) then
        call stat_check(modflow%update(), stat)
     end if

     ! transfer variables
     call stat_check(modflow%get_value("foo", foo_data), stat)
     call stat_check(wrf_hydro%set_value("soldrain", foo_data), stat)

     call stat_check(wrf_hydro%get_current_time(current_time), stat)
  end do


  call stat_check(wrf_hydro%finalize(), stat)
  ! call stat_check(modflow%finalize(), stat)
  print *, "--- FIN ---"

end program wrf_hydro_nwm_bmi_driver
