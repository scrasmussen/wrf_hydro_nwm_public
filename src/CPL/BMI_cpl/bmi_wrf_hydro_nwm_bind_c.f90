submodule (bmi_wrf_hydro_nwm_bind_c_mod) bmi_wrf_hydro_nwm_smod
  implicit none

  integer(c_int), parameter :: STUB_I = -1
  integer(c_int), dimension(1), parameter :: STUB_1D_I = [-1]
  real(c_double), parameter :: STUB_D = -1.0
  real(c_double), dimension(1), parameter :: STUB_1D_D = [-1.0]
  integer(c_int), parameter :: STUB_C_LEN = 11
  character(len=STUB_C_LEN), parameter :: STUB_C = "STUB RESULT"

contains

  ! Get the name of the model.
  module procedure wrf_hydro_component_name_c
    use module_version, only : get_model_version
    character(len=BMI_MAX_COMPONENT_NAME), target :: model_name
    model_name = get_model_version()
    name(1:BMI_MAX_COMPONENT_NAME) = f_to_c_str(model_name)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_component_name

  ! Perform startup tasks for the model.
  module procedure wrf_hydro_initialize_c
    bmi_status = wrf_hydro%initialize("no config file")
  end procedure ! wrf_hydro_initialize

  ! Advance the model one time step.
  module procedure wrf_hydro_update_c
    bmi_status = wrf_hydro%update()
  end procedure ! wrf_hydro_update

  ! Perform teardown tasks for the model.
  module procedure wrf_hydro_finalize_c
    bmi_status = wrf_hydro%finalize()
  end procedure ! wrf_hydro_finalize

  ! Advance the model until the given time.
  module procedure wrf_hydro_update_until_c
    bmi_status = wrf_hydro%update_until(time)
  end procedure ! wrf_hydro_update_until

  ! Count a model's input variables.
  module procedure wrf_hydro_input_item_count_c
    bmi_status = wrf_hydro%get_input_item_count(count)
  end procedure ! wrf_hydro_input_item_count

  ! Count a model's output variables.
  module procedure wrf_hydro_output_item_count_c
    bmi_status = wrf_hydro%get_output_item_count(count)
  end procedure ! wrf_hydro_output_item_count

  ! Start time of the model.
  module procedure wrf_hydro_start_time_c
    bmi_status = wrf_hydro%get_start_time(time)
  end procedure ! wrf_hydro_start_time

  ! End time of the model.
  module procedure wrf_hydro_end_time_c
    bmi_status = wrf_hydro%get_end_time(time)
  end procedure ! wrf_hydro_end_time

  ! Time step of the model.
  module procedure wrf_hydro_time_step_c
    bmi_status = wrf_hydro%get_end_time(time_step)
  end procedure ! wrf_hydro_time_step

  ! Current time of the model.
  module procedure wrf_hydro_current_time_c
    bmi_status = wrf_hydro%get_current_time(time)
  end procedure ! wrf_hydro_current_time

  !---------------------------------------------------------------------
  ! STUBS: Section consists of stubs to allow building and testing.
  !        Move above when implemented.
  !---------------------------------------------------------------------

  ! List a model's input variables.
  module procedure wrf_hydro_input_var_names_c
    character(len=BMI_MAX_VAR_NAME), pointer :: var_name(:)
    ! character(len=BMI_MAX_COMPONENT_NAME), target :: var_name
    bmi_status = wrf_hydro%get_input_var_names(var_name)
    ! -----TODO-----
    ! names(1:BMI_MAX_COMPONENT_NAME) = f_to_c_str(var_names)
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_input_var_names

  ! List a model's output variables.
  module procedure wrf_hydro_output_var_names_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_output_var_names

  ! Get the grid identifier for the given variable.
  module procedure wrf_hydro_var_grid_c
    character(len=BMI_MAX_VAR_NAME), pointer :: var_name(:)
    ! character(len=BMI_MAX_VAR_NAME), allocatable :: var_name(:)
    bmi_status = BMI_SUCCESS
    var_name = name(1:BMI_MAX_VAR_NAME)
    ! -----TODO-----
    ! bmi_status = wrf_hydro%get_var_grid(var_name, grid)
    ! print*, "F name", name(1:4)
    grid = 8
  end procedure ! wrf_hydro_var_grid

  ! Get the data type of the given variable as a string.
  module procedure wrf_hydro_var_type_c
    type(1:STUB_C_LEN) = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_type

  ! Get the units of the given variable.
  module procedure wrf_hydro_var_units_c
    units(1:STUB_C_LEN) = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_units

  ! Get memory use per array element, in bytes.
  module procedure wrf_hydro_var_itemsize_c
    size = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_itemsize

  ! Get size of the given variable, in bytes.
  module procedure wrf_hydro_var_nbytes_c
    nbytes = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_nbytes

  ! Describe where a variable is located: node, edge, or face.
  module procedure wrf_hydro_var_location_c
    location(1:STUB_C_LEN) = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_location

  ! Time units of the model.
  module procedure wrf_hydro_time_units_c
    units(1:STUB_C_LEN) = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_time_units

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure wrf_hydro_get_int_c
    character(len=BMI_MAX_COMPONENT_NAME) :: f_str
    f_str = c_to_f_str(name)
    bmi_status = wrf_hydro%get_value_int(f_str, dest)
  end procedure ! wrf_hydro_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure wrf_hydro_get_float_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure wrf_hydro_get_double_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_double

  ! Get a reference to the given integer variable.
  module procedure wrf_hydro_get_ptr_int_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_int

  ! Get a reference to the given real variable.
  module procedure wrf_hydro_get_ptr_float_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_float

  ! Get a reference to the given double variable.
  module procedure wrf_hydro_get_ptr_double_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_double

  ! Get integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_int_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_int

  ! Get real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_float_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_float

  ! Get double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_double_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_double

  ! Set new values for an integer model variable.
  module procedure wrf_hydro_set_int_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_int

  ! Set new values for a real model variable.
  module procedure wrf_hydro_set_float_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_float

  ! Set new values for a double model variable.
  module procedure wrf_hydro_set_double_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_double

  ! Set integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_int_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_float_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_double_c
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_double

  ! Get number of dimensions of the computational grid.
  module procedure wrf_hydro_grid_rank_c
    bmi_status = wrf_hydro%get_grid_rank(grid, rank)
  end procedure ! wrf_hydro_grid_rank

  ! Get the total number of elements in the computational grid.
  module procedure wrf_hydro_grid_size_c
    bmi_status = wrf_hydro%get_grid_size(grid, size)
  end procedure ! wrf_hydro_grid_size

  ! Get the grid type as a string.
  module procedure wrf_hydro_grid_type_c
    type(1:STUB_C_LEN) = trim(STUB_C)
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_type

  ! Get the dimensions of the computational grid.
  module procedure wrf_hydro_grid_shape_c
    integer, allocatable :: grid_shape(:)
    bmi_status = wrf_hydro%get_grid_shape(grid, grid_shape)
    shape = grid_shape
  end procedure ! wrf_hydro_grid_shape

  ! Get distance between nodes of the computational grid.
  module procedure wrf_hydro_grid_spacing_c
    spacing = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_spacing

  ! Get coordinates of the origin of the computational grid.
  module procedure wrf_hydro_grid_origin_c
    origin = STUB_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_origin

  ! Get the x-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_x_c
    x = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_x

  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_y_c
    y = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_y

  ! Get the z-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_z_c
    z = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_z

  ! Get the number of nodes in an unstructured grid.
  module procedure wrf_hydro_grid_node_count_c
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_node_count

  ! Get the number of edges in an unstructured grid.
  module procedure wrf_hydro_grid_edge_count_c
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  module procedure wrf_hydro_grid_face_count_c
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_count

  ! Get the edge-node connectivity.
  module procedure wrf_hydro_grid_edge_nodes_c
    edge_nodes = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_edge_nodes

  ! Get the face-edge connectivity.
  module procedure wrf_hydro_grid_face_edges_c
    face_edges = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_edges

  ! Get the face-node connectivity.
  module procedure wrf_hydro_grid_face_nodes_c
    face_nodes = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_nodes

  ! Get the number of nodes for each face.
  module procedure wrf_hydro_grid_nodes_per_face_c
    nodes_per_face = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_nodes_per_face


  ! ------------------------------------
  ! Non-BMI procedures
  ! ------------------------------------

  ! convert Fortran character array to C character array
  function f_to_c_str(f_str) result(c_str)
    character(len=BMI_MAX_COMPONENT_NAME) :: f_str
    character(c_char) :: c_str(BMI_MAX_COMPONENT_NAME)
    integer :: i, name_len
    name_len = len(trim(f_str))
    do i=1,name_len
       c_str(i) = f_str(i:i)
    end do
    c_str(name_len+1) = C_NULL_CHAR
  end function f_to_c_str

  ! TODO: look at wrf_hydro_get_int_c
  ! convert Fortran character array to C character array
  function c_to_f_str(c_str) result(f_str)
    character(c_char) :: c_str(BMI_MAX_COMPONENT_NAME)
    character(len=BMI_MAX_COMPONENT_NAME) :: f_str
    integer :: i, name_len
    ! name_len = index(c_str, C_NULL_CHAR)
    name_len = 0!index(c_str, C_NULL_CHAR)
    ! print *, "name_len" , name_len, "|", index(c_str, C_NULL_CHAR)
    do i=1,name_len
       f_str(i:i) = c_str(i)
    end do
  end function c_to_f_str

  ! Model introspection.
  module procedure print_model_info_c
  end procedure ! print_model_info

  ! Check the status and update bmi_status if necessary
  module procedure stat_check_c
    if (status .ne. BMI_SUCCESS) then
       bmi_status = BMI_FAILURE
    end if
  end procedure

end submodule bmi_wrf_hydro_nwm_smod
