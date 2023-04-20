submodule (bmi_wrf_hydro_nwm_mod) bmi_wrf_hydro_nwm_smod
  implicit none

  integer, parameter :: STUB_I = -1
  integer, dimension(1), parameter :: STUB_1D_I = [-1]
  double precision, parameter :: STUB_D = -1.0
  double precision, dimension(1), parameter :: STUB_1D_D = [-1.0]
  character(len=11), parameter :: STUB_C = "STUB RESULT"

contains

  ! Get the name of the model.
  module procedure wrf_hydro_component_name
    use module_version, only : get_model_version
    character(len=1024), target :: model_name
    model_name = get_model_version()
    name => model_name
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_component_name

  ! Perform startup tasks for the model.
  module procedure wrf_hydro_initialize
    use orchestrator_base, only : orchestrator
    use module_noahmp_hrldas_driver, only: land_driver_ini
    integer :: end_time
    double precision :: start_time
    bmi_status = BMI_SUCCESS
    call orchestrator%init()
    this%orchestrator = orchestrator
    call land_driver_ini(end_time, this%model)

    ! initialize time step values
    call stat_check(this%get_start_time(start_time), bmi_status)
    this%model%itimestep = int(start_time)
    this%model%ntime = end_time
    this%model%timestep = 1
  end procedure ! wrf_hydro_initialize

  ! Advance the model one time step.
  module procedure wrf_hydro_update
    use module_noahmp_hrldas_driver, only: land_driver_exe
    double precision :: current_time, time_step
    bmi_status = BMI_SUCCESS
    call stat_check(this%get_current_time(current_time), bmi_status)
    call stat_check(this%get_time_step(time_step), bmi_status)
    call land_driver_exe(int(current_time), this%model)
    this%model%itimestep = this%model%itimestep + int(time_step)
  end procedure ! wrf_hydro_update

  !---------------------------------------------------------------------
  ! Should update how this is handeled in main_hrldas_driver
  !---------------------------------------------------------------------
  ! Start time of the model.
  module procedure wrf_hydro_start_time
    time = 1.0
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_start_time

  ! End time of the model.
  module procedure wrf_hydro_end_time
    time = dble(this%model%ntime)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_end_time

  ! Time step of the model.
  module procedure wrf_hydro_time_step
    time_step = dble(this%model%timestep)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_time_step

  ! Current time of the model.
  module procedure wrf_hydro_current_time
    time = dble(this%model%itimestep)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_current_time


  !---------------------------------------------------------------------
  ! STUBS: Section consists of stubs to allow building and testing.
  !        Move above when implemented.
  !---------------------------------------------------------------------

  ! Perform teardown tasks for the model.
  module procedure wrf_hydro_finalize
    ! use module_hydro_drv!, only : hydro_finish
    call hydro_finish()
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_finalize

  ! Advance the model until the given time.
  module procedure wrf_hydro_update_until
    double precision :: current_time
    bmi_status = BMI_SUCCESS
    call stat_check(this%get_current_time(current_time), bmi_status)
    do while (current_time < time)
       call stat_check(this%update(), bmi_status)
       call stat_check(this%get_current_time(current_time), bmi_status)
    end do
  end procedure ! wrf_hydro_update_until

  ! Count a model's input variables.
  module procedure wrf_hydro_input_item_count
    count = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_input_item_count

  ! Count a model's output variables.
  module procedure wrf_hydro_output_item_count
    count = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_output_item_count

  ! List a model's input variables.
  module procedure wrf_hydro_input_var_names
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_input_var_names

  ! List a model's output variables.
  module procedure wrf_hydro_output_var_names
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_output_var_names

  ! Get the grid identifier for the given variable.
  module procedure wrf_hydro_var_grid
    grid = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_grid

  ! Get the data type of the given variable as a string.
  module procedure wrf_hydro_var_type
    type = STUB_C
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_type

  ! Get the units of the given variable.
  module procedure wrf_hydro_var_units
    units = STUB_C
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_units

  ! Get memory use per array element, in bytes.
  module procedure wrf_hydro_var_itemsize
    size = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_itemsize

  ! Get size of the given variable, in bytes.
  module procedure wrf_hydro_var_nbytes
    nbytes = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_nbytes

  ! Describe where a variable is located: node, edge, or face.
  module procedure wrf_hydro_var_location
    location = STUB_C
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_location

  ! Time units of the model.
  module procedure wrf_hydro_time_units
    units = STUB_C
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_time_units

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure wrf_hydro_get_int
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure wrf_hydro_get_float
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure wrf_hydro_get_double
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_double

  ! Get a reference to the given integer variable.
  module procedure wrf_hydro_get_ptr_int
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_ptr_int

  ! Get a reference to the given real variable.
  module procedure wrf_hydro_get_ptr_float
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_ptr_float

  ! Get a reference to the given double variable.
  module procedure wrf_hydro_get_ptr_double
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_ptr_double

  ! Get integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_int
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_at_indices_int

  ! Get real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_float
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_at_indices_float

  ! Get double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_double
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_get_at_indices_double

  ! Set new values for an integer model variable.
  module procedure wrf_hydro_set_int
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_int

  ! Set new values for a real model variable.
  module procedure wrf_hydro_set_float
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_float

  ! Set new values for a double model variable.
  module procedure wrf_hydro_set_double
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_double

  ! Set integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_int
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_float
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_double
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_set_at_indices_double

  ! Get number of dimensions of the computational grid.
  module procedure wrf_hydro_grid_rank
    rank = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_rank

  ! Get the total number of elements in the computational grid.
  module procedure wrf_hydro_grid_size
    size = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_size

  ! Get the grid type as a string.
  module procedure wrf_hydro_grid_type
    type = STUB_C
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_type

  ! Get the dimensions of the computational grid.
  module procedure wrf_hydro_grid_shape
    shape = STUB_1D_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_shape

  ! Get distance between nodes of the computational grid.
  module procedure wrf_hydro_grid_spacing
    spacing = STUB_1D_D
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_spacing

  ! Get coordinates of the origin of the computational grid.
  module procedure wrf_hydro_grid_origin
    origin = STUB_D
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_origin

  ! Get the x-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_x
    x = STUB_1D_D
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_x

  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_y
    y = STUB_1D_D
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_y

  ! Get the z-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_z
    z = STUB_1D_D
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_z

  ! Get the number of nodes in an unstructured grid.
  module procedure wrf_hydro_grid_node_count
    count = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_node_count

  ! Get the number of edges in an unstructured grid.
  module procedure wrf_hydro_grid_edge_count
    count = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  module procedure wrf_hydro_grid_face_count
    count = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_face_count

  ! Get the edge-node connectivity.
  module procedure wrf_hydro_grid_edge_nodes
    edge_nodes = STUB_1D_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_edge_nodes

  ! Get the face-edge connectivity.
  module procedure wrf_hydro_grid_face_edges
    face_edges = STUB_1D_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_face_edges

  ! Get the face-node connectivity.
  module procedure wrf_hydro_grid_face_nodes
    face_nodes = STUB_1D_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_face_nodes

  ! Get the number of nodes for each face.
  module procedure wrf_hydro_grid_nodes_per_face
    nodes_per_face = STUB_I
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_grid_nodes_per_face


  ! ------------------------------------
  ! Non-BMI procedures
  ! ------------------------------------

  ! Model introspection.
  module procedure print_model_info
  end procedure ! print_model_info

  ! Check the status and update bmi_status if necessary
  module procedure stat_check
    if (status .ne. BMI_SUCCESS) then
       bmi_status = BMI_FAILURE
    end if
  end procedure
end submodule bmi_wrf_hydro_nwm_smod
