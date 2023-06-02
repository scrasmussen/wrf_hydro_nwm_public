program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use bmi_modflow_mod, only : modflow6, bmi_modflow, BMI_LENCOMPONENTNAME
  use iso_c_binding, only : c_char, C_NULL_CHAR
  use mf6bmiUtil, only:  BMI_LENVARADDRESS
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
  integer :: mf_input_count, mf_output_count
  character(len=BMI_MAX_VAR_NAME), pointer :: names(:)

  ! soldrain
  integer :: soldrain_grid, soldrain_rank, soldrain_size
  integer, allocatable :: soldrain_grid_shape(:)
  integer :: soldrain_grid_shape_const(2)
  real, allocatable :: soldrain(:,:), soldrain_flat(:)


  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  modflow = modflow6()
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  call stat_check(modflow%get_component_name(mf_model_name), stat)

  print * , "--- Starting ", trim(model_name), " and ", trim(mf_model_name), " ---"

  ! initialize model
  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(modflow%initialize(""), stat)

  ! get timing components
  call stat_check(wrf_hydro%get_start_time(current_time), stat)
  call stat_check(wrf_hydro%get_end_time(end_time), stat)
  call stat_check(wrf_hydro%get_time_step(time_step), stat)
  call stat_check(modflow%get_time_step(mf_time_step), stat)
  call stat_check(wrf_hydro%get_time_units(time_unit), stat)
  ! call stat_check(modflow%get_time_units(mf_time_unit), stat) ! hardcoded, need to update

  ! --- setup soldrain variables
  call stat_check(wrf_hydro%get_var_grid("soldrain", soldrain_grid), stat)
  call stat_check(wrf_hydro%get_grid_rank(soldrain_grid, soldrain_rank), stat)
  call stat_check(wrf_hydro%get_grid_shape(soldrain_grid, soldrain_grid_shape), stat)
  soldrain_grid_shape_const = soldrain_grid_shape
  call stat_check(wrf_hydro%get_grid_size(soldrain_grid, soldrain_size), stat)
  allocate(soldrain_flat(soldrain_size))
  allocate(soldrain(soldrain_grid_shape(1), soldrain_grid_shape(2)))
  ! --- done setting up soldrain variables


  do while (current_time < end_time)
     ! update models
     call stat_check(wrf_hydro%update(), stat)
     call stat_check(modflow%update(), stat)
     ! the math for when to update modflow needs to be checked
     ! if (mod(current_time, mf_time_step) == 0) then
     !    call stat_check(modflow%update(), stat)
     ! end if

     ! --- transfer variables
     ! get current values
     ! call stat_check(modflow%get_value("foo", foo_data), stat)
     call stat_check(wrf_hydro%get_value("soldrain", soldrain_flat), stat)
     soldrain = reshape(soldrain_flat, soldrain_grid_shape_const)

     ! update soldrain value
     ! soldrain = soldrain + update_value
     call stat_check(wrf_hydro%set_value("soldrain", pack(soldrain, .true.)), stat)

     ! update current_time
     call stat_check(wrf_hydro%get_current_time(current_time), stat)
  end do


  call stat_check(wrf_hydro%finalize(), stat)
  call stat_check(modflow%finalize(), stat)
  print *, "--- FIN ---"

end program wrf_hydro_nwm_bmi_driver
