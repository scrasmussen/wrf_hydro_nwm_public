program wrf_hydro_nwm_bmi_driver
  use bmi_wrf_hydro_nwm_mod, only: bmi_wrf_hydro_nwm, stat_check!, BMI_SUCCESS
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_COMPONENT_NAME, BMI_MAX_VAR_NAME
  use bmi_wrf_hydro_nwm_mod, only: BMI_MAX_TYPE_NAME, BMI_MAX_UNITS_NAME
  use bmi_wrf_hydro_nwm_mod, only: wrf_hydro_nwm
  use mf6bmi
  ! use bmi_modflow_mod, only: modflow6, bmi_modflow
  implicit none
  ! type(bmi_modflow) :: modflow

  type(bmi_wrf_hydro_nwm) :: wrf_hydro
  character(len=BMI_MAX_COMPONENT_NAME), pointer :: model_name
  character(len=BMI_MAX_VAR_NAME) :: time_unit
  character(c_char) :: mf_model_name(256) ! BMI_LENCOMPONENTNAME
  double precision :: end_time, current_time, time_step
  integer :: res, stat, i
  integer, allocatable :: end(:), var_data(:), grid_shape(:)
  integer, allocatable :: ind_data(:), indices_data(:), indices(:)
  integer, pointer ::  ptr_data(:)
  stat = BMI_SUCCESS

  wrf_hydro = wrf_hydro_nwm()
  ! modflow = modflow6()
  call stat_check(wrf_hydro%get_component_name(model_name), stat)
  call stat_check(bmi_get_component_name(mf_model_name), stat)
  end = findloc(mf_model_name, C_NULL_CHAR)

  print * , "--- Starting ", trim(model_name), " and ", &
       mf_model_name(1:end(1)), " ---"

  call stat_check(wrf_hydro%initialize("no config file"), stat)
  call stat_check(bmi_initialize(), stat)


  call stat_check(wrf_hydro%finalize(), stat)
  call stat_check(bmi_finalize(), stat)
  print *, "--- FIN ---"








  ! ! call stat_check(wrf_hydro%get_start_time(current_time), stat)
  ! ! call stat_check(wrf_hydro%get_end_time(end_time), stat)

  ! call stat_check(wrf_hydro%get_time_units(time_unit), stat)
  ! print *, "time unit is: ", trim(time_unit)

  ! call stat_check(wrf_hydro%get_input_item_count(input_item_count), stat)
  ! call stat_check(wrf_hydro%get_input_var_names(input_var_names), stat)
  ! do i=1,input_item_count
  !    print *, "input item", i, ":", trim(input_var_names(i))
  ! end do

  ! call stat_check(wrf_hydro%get_output_item_count(output_item_count), stat)
  ! call stat_check(wrf_hydro%get_output_var_names(output_var_names), stat)
  ! do i=1,output_item_count
  !    print *, "output item", i, ":", trim(output_var_names(i))
  ! end do

  ! call stat_check(wrf_hydro%get_var_grid("IVGTYP", var_grid), stat)
  ! ! res = wrf_hydro%get_var_grid("ISLTYP", var_grid)

  ! call stat_check(wrf_hydro%get_var_type(trim(output_var_names(var_grid)), type_name), stat)
  ! call stat_check(wrf_hydro%get_var_units(trim(output_var_names(var_grid)), var_unit), stat)
  ! call stat_check(wrf_hydro%get_grid_size(var_grid, grid_size), stat)
  ! call stat_check(wrf_hydro%get_var_itemsize(trim(output_var_names(var_grid)), item_size), stat)
  ! call stat_check(wrf_hydro%get_var_nbytes(trim(output_var_names(var_grid)), nbytes), stat)

  ! print *, "var ", trim(output_var_names(var_grid)), " is of type ", trim(type_name), &
  !      " and has units ", trim(var_unit)
  ! print *, "var ", trim(output_var_names(var_grid)), " is grid", var_grid, &
  !      " also has grid size", grid_size, "has item size", item_size, "so naturally nbytes of", nbytes


  ! allocate(var_data(grid_size))
  ! call stat_check(wrf_hydro%get_value(trim(output_var_names(var_grid)), var_data), stat)
  ! print *, "getting data", var_data(228:230), "should match 11, 5, 11"
  ! print *, "-----", stat
  ! call stat_check(wrf_hydro%get_grid_shape(var_grid, grid_shape), stat)
  ! print *, "Shape is ", grid_shape, " and stat" ,stat

  ! print *, "====== PTR TEST ======"
  ! call stat_check(wrf_hydro%get_value_ptr(trim(output_var_names(var_grid)), ptr_data), stat)
  ! stat = BMI_SUCCESS

  ! print *, "ptr_data associated value is ", associated(ptr_data)

  ! print *, "====== GET AT INDICES ======"

  ! ! ind_data: need to allocate?
  ! indices = [228,230,229]
  ! allocate(ind_data(size(indices)))
  ! call stat_check(wrf_hydro%get_value_at_indices(trim(output_var_names(var_grid)), ind_data, &
  !      [228,230,229]), stat)

  ! print *, "got the data from indices:", ind_data

  ! print *, "====== SETTING DATA ======"
  ! var_data = -77
  ! call stat_check(wrf_hydro%set_value(trim(output_var_names(var_grid)), var_data), stat)
  ! print *, "-- now by indices"
  ! indices = [2,4,20]
  ! indices_data = [11., 12., 13.]
  ! call stat_check(wrf_hydro%set_value_at_indices(trim(output_var_names(var_grid)), &
  !      indices, indices_data), stat)


  ! do while (current_time < end_time)
  !    call stat_check(wrf_hydro%update(), stat)
  !    call stat_check(wrf_hydro%get_current_time(current_time), stat)
  ! end do

  ! print *, "--- Finished running test driver ---"
  ! if (stat .ne. BMI_SUCCESS) then
  !    print *, "WRF-Hydro driver returned failure"
  !    error stop stat
  ! end if
end program wrf_hydro_nwm_bmi_driver
