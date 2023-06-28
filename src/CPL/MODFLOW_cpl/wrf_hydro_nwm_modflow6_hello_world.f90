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
  double precision :: end_time, current_time, mf_current_time
  double precision :: time_step, mf_time_step, time_step_conv
  integer :: i, bmi_status

  ! soldrain
  integer :: soldrain_grid, soldrain_rank, soldrain_size
  real, allocatable :: soldrain(:,:), soldrain_flat(:)
  integer, allocatable :: soldrain_grid_shape(:)
  integer :: soldrain_grid_shape_const(2)

  ! modflow
  integer :: modflow_output_item_count
  integer :: x_grid, x_rank, x_size
  integer, allocatable :: x_grid_shape(:)
  integer :: x_grid_shape_const(1)
  double precision, allocatable :: x(:,:), x_flat(:)


  wrf_hydro = wrf_hydro_nwm()
  modflow = modflow6()
  call stat_check(wrf_hydro%get_component_name(model_name))
  call stat_check(modflow%get_component_name(mf_model_name))

  ! print * , "--- Starting ", trim(model_name), " and ", trim(mf_model_name), " ---"

  ! initialize model
  call stat_check(wrf_hydro%initialize("no config file"))
  call stat_check(modflow%initialize(""))

  ! get timing components
  call stat_check(wrf_hydro%get_start_time(current_time))
  call stat_check(wrf_hydro%get_end_time(end_time))
  call stat_check(wrf_hydro%get_time_step(time_step))
  call stat_check(modflow%get_time_step(mf_time_step))
  call stat_check(wrf_hydro%get_time_units(time_unit))
  call stat_check(wrf_hydro%get_start_time(mf_current_time))
  ! call stat_check(modflow%get_time_units(mf_time_unit)) ! hardcoded, need to update
  ! call stat_check(modflow%get_output_item_count(modflow_output_item_count))
  print *, "time steps:", time_step, mf_time_step

  ! print *, "modflow_output_item_count", modflow_output_item_count

  ! --- setup soldrain variables
  call stat_check(wrf_hydro%get_var_grid("soldrain", soldrain_grid))
  call stat_check(wrf_hydro%get_grid_rank(soldrain_grid, soldrain_rank))
  call stat_check(wrf_hydro%get_grid_shape(soldrain_grid, soldrain_grid_shape))
  soldrain_grid_shape_const = soldrain_grid_shape
  call stat_check(wrf_hydro%get_grid_size(soldrain_grid, soldrain_size))
  allocate(soldrain_flat(soldrain_size))
  allocate(soldrain(soldrain_grid_shape(1), soldrain_grid_shape(2)))
  ! --- done setting up soldrain variables

  ! --- setup modflow x variable
  call stat_check(modflow%get_var_grid("X", x_grid))
  call stat_check(modflow%get_grid_rank(x_grid, x_rank))
  call stat_check(modflow%get_grid_shape(x_grid, x_grid_shape))
  x_grid_shape_const = x_grid_shape
  call stat_check(modflow%get_grid_size(x_grid, x_size))
  allocate(x_flat(x_size))
  ! allocate(x(x_grid_shape(1), x_grid_shape(2)))
  ! --- done setting up x variable

  end_time = 4
  print *, "TESTING: Setting end_time to", end_time

  do while (current_time < end_time)
     ! update models
     call stat_check(wrf_hydro%update())
     call stat_check(modflow%update())
     ! update current_time
     call stat_check(wrf_hydro%get_current_time(current_time))
     call stat_check(modflow%get_current_time(mf_current_time))
     call stat_check(modflow%get_time_step(mf_time_step))
     time_step_conv = time_step / mf_time_step



     ! get current values
     call stat_check(modflow%get_value("X", x_flat))
     ! x = reshape(x_flat, x_grid_shape_const)

     do while (current_time < mf_current_time .and. &
          current_time < end_time)
        call stat_check(wrf_hydro%get_current_time(current_time))
        call stat_check(modflow%get_current_time(mf_current_time))
        time_step_conv = time_step / mf_time_step
        print *, "[", int(current_time), "/", int(mf_current_time), "]", &
             " time_steps =", real(time_step), real(mf_time_step), &
             "conv", real(time_step_conv)

        call stat_check(wrf_hydro%get_value("soldrain", soldrain_flat))
        soldrain = reshape(soldrain_flat, soldrain_grid_shape_const)

        ! --- update soldrain value
        ! soldrain = x_flat * time_step_conv
        soldrain = soldrain + 0.01 ! test update_value
        call stat_check(wrf_hydro%set_value("soldrain", pack(soldrain, .true.)))

        ! update current_time
        call stat_check(wrf_hydro%update())
     end do

  end do


  call stat_check(modflow%finalize())
  call stat_check(wrf_hydro%finalize())
  print *, "--- FIN ---"

end program wrf_hydro_nwm_bmi_driver
