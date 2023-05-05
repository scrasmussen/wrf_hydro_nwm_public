submodule (bmi_wrf_hydro_nwm_mod) bmi_wrf_hydro_nwm_smod
  implicit none

  integer, parameter :: STUB_I = -1
  integer, dimension(1), parameter :: STUB_1D_I = [-1]
  double precision, parameter :: STUB_D = -1.0
  double precision, dimension(1), parameter :: STUB_1D_D = [-1.0]
  character(len=11), parameter :: STUB_C = "STUB RESULT"

contains

  ! --- BMI Function Implementation ---
  ! Get the name of the model.
  module procedure wrf_hydro_component_name
    use module_version, only : get_model_version
    ! TODO: this save seemed to be needed, check if this is true
    character(len=BMI_MAX_COMPONENT_NAME), target, save :: model_name
    model_name = get_model_version()
    name => model_name
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_component_name

  ! Perform startup tasks for the model.
  module procedure wrf_hydro_initialize
    use orchestrator_base, only : orchestrator
    use module_noahmp_hrldas_driver, only : land_driver_ini
    use module_NWM_io_dict, only : initLdasDict
    integer :: end_time
    double precision :: start_time
    integer :: procID, diagFlag
    bmi_status = BMI_SUCCESS
    call orchestrator%init()
    this%orchestrator = orchestrator
    call land_driver_ini(end_time, this%model)

    ! initialize time step values
    call stat_check(this%get_start_time(start_time), bmi_status)
    this%model%itimestep = int(start_time)
    this%model%ntime = end_time
    this%model%timestep = 1

    procID = 1
    ! TODO: this is done twice, inefficient, save ldasOutDict first time
    call initLdasDict(ldasOutDict,procID,diagFlag)
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
    count = input_item_count
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_input_item_count

  ! Count a model's output variables.
  module procedure wrf_hydro_output_item_count
    count = output_item_count
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_output_item_count

  ! Get memory use per array element, in bytes.
  module procedure wrf_hydro_var_itemsize
    integer :: res
    integer :: integer_var
    real :: real_var
    double precision :: double_var
    logical :: logical_var
    character(BMI_MAX_TYPE_NAME) :: var_type
    bmi_status = BMI_SUCCESS
    res = this%get_var_type(name, var_type)
    select case(var_type)
    case("integer")
       size = sizeof(integer_var)
    case("real")
       size = sizeof(real_var)
    case("double precision")
       size = sizeof(double_var)
    case("logical")
       size = sizeof(logical_var)
    case default
       size = 0
       bmi_status = BMI_FAILURE
    end select
  end procedure ! wrf_hydro_var_itemsize

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

  ! Time units of the model.
  module procedure wrf_hydro_time_units
    units = "h"
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_time_units

  ! Current time of the model.
  module procedure wrf_hydro_current_time
    time = dble(this%model%itimestep)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_current_time

  ! Get size of the given variable, in bytes.
  module procedure wrf_hydro_var_nbytes
    integer :: res, grid, var_size, var_rank, i
    integer :: item_size, grid_size
    bmi_status = BMI_SUCCESS
    res = this%get_var_grid(name, grid)
    res = this%get_var_itemsize(name, item_size)
    res = this%get_grid_size(grid, grid_size)
    nbytes = item_size * grid_size
    if (nbytes == 0) bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_nbytes

  !---------------------------------------------------------------------
  ! Implemented but should be improved
  !---------------------------------------------------------------------
  ! TODO: the following implementations are first takes for ldas, how do we add
  !       the other parts of the model?
  ! List a model's output variables.
  module procedure wrf_hydro_output_var_names
    allocate(names(output_item_count))
    names = ldasOutDict%varNames
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_output_var_names

  ! Get the data type of the given variable as a string.
  module procedure wrf_hydro_var_type
    integer :: grid, res
    character(:), allocatable :: var_type
    res = this%get_var_grid(name, grid)
    var_type = get_unit_str(ldasOutDict%var_type(grid))
    type = var_type
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_type

  module procedure wrf_hydro_var_units
    integer :: grid, res
    res = this%get_var_grid(name, grid)
    units = ldasOutDict%units(grid)
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_var_units

  ! Get the grid identifier for the given variable.
  module procedure wrf_hydro_var_grid
    bmi_status = BMI_SUCCESS
    select case(name)
    case("Fortran indexing starts at 1")
       grid = 0
    case("IVGTYP")
       grid = 1
    case("ISLTYP")
       grid = 2
    case default
       grid = -1
       bmi_status = BMI_FAILURE
       print *, "WARNING: variable ", trim(name), " not found"
    end select
  end procedure ! wrf_hydro_var_grid

  ! this function is need so that there isn't an object clash between the
  ! variable size and the intrinsic function size
  function get_var_size(grid) result(var_size)
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    integer, intent(in) :: grid
    integer :: var_size
    select case(grid)
    case(0)
       var_size = 0
    case(1) ! "IVGTYP"
       var_size = size(IVGTYP)
       print *, "-------", size(IVGTYP), "|",shape(IVGTYP)
    case(2) ! "ISLTYP"
       var_size = size(ISLTYP)
    case default
       var_size = 0
       print *, "WARNING: variable ", grid, " not found"
    end select
  end function get_var_size

  ! Get the total number of elements in the computational grid.
  module procedure wrf_hydro_grid_size
    bmi_status = BMI_SUCCESS
    size = get_var_size(grid)
    if (size == 0) bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_size


  !---------------------------------------------------------------------
  ! STUBS: Section consists of stubs to allow building and testing.
  !        Move above when implemented.
  !---------------------------------------------------------------------

  ! List a model's input variables.
  module procedure wrf_hydro_input_var_names
    allocate(names(2))
    names(1) = output_item_list(1)
    names(2) = output_item_list(2)
    ! names => input_item_list
    bmi_status = BMI_SUCCESS
  end procedure ! wrf_hydro_input_var_names

  ! Describe where a variable is located: node, edge, or face.
  module procedure wrf_hydro_var_location
    location = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_var_location

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure wrf_hydro_get_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure wrf_hydro_get_float
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    integer :: res, grid, size
    real, allocatable ::  var_data(:)
    res = this%get_var_grid(name, grid)
    select case(grid)
    case(0)
       ! dest =
    case(1) ! "IVGTYP"
       ! if (allocated(dest)) then
       !    var_data = pack(IVGTYP, .true.)
       ! else
       !    stop error "dest is not allocated"
       ! end if
       ! var_data
       dest = pack(IVGTYP, .true.)
    case(2) ! "ISLTYP"
       var_data = pack(ISLTYP, .true.)
       dest = var_data
    case default
       print *, "WARNING: ", trim(name), " data not found"
    end select

    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure wrf_hydro_get_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_double

  ! Get a reference to the given integer variable.
  module procedure wrf_hydro_get_ptr_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_int

  ! Get a reference to the given real variable.
  module procedure wrf_hydro_get_ptr_float
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_float

  ! Get a reference to the given double variable.
  module procedure wrf_hydro_get_ptr_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_double

  ! Get integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_int

  ! Get real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_float

  ! Get double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_double

  ! Set new values for an integer model variable.
  module procedure wrf_hydro_set_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_int

  ! Set new values for a real model variable.
  module procedure wrf_hydro_set_float
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_float

  ! Set new values for a double model variable.
  module procedure wrf_hydro_set_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_double

  ! Set integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_set_at_indices_double

  ! Get number of dimensions of the computational grid.
  module procedure wrf_hydro_grid_rank
    rank = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_rank

  ! Get the grid type as a string.
  module procedure wrf_hydro_grid_type
    type = STUB_C
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_type

  ! Get the dimensions of the computational grid.
  module procedure wrf_hydro_grid_shape
    shape = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_shape

  ! Get distance between nodes of the computational grid.
  module procedure wrf_hydro_grid_spacing
    spacing = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_spacing

  ! Get coordinates of the origin of the computational grid.
  module procedure wrf_hydro_grid_origin
    origin = STUB_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_origin

  ! Get the x-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_x
    x = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_x

  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_y
    y = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_y

  ! Get the z-coordinates of the nodes of a computational grid.
  module procedure wrf_hydro_grid_z
    z = STUB_1D_D
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_z

  ! Get the number of nodes in an unstructured grid.
  module procedure wrf_hydro_grid_node_count
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_node_count

  ! Get the number of edges in an unstructured grid.
  module procedure wrf_hydro_grid_edge_count
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  module procedure wrf_hydro_grid_face_count
    count = STUB_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_count

  ! Get the edge-node connectivity.
  module procedure wrf_hydro_grid_edge_nodes
    edge_nodes = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_edge_nodes

  ! Get the face-edge connectivity.
  module procedure wrf_hydro_grid_face_edges
    face_edges = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_edges

  ! Get the face-node connectivity.
  module procedure wrf_hydro_grid_face_nodes
    face_nodes = STUB_1D_I
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_face_nodes

  ! Get the number of nodes for each face.
  module procedure wrf_hydro_grid_nodes_per_face
    nodes_per_face = STUB_I
    bmi_status = BMI_FAILURE
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

  function get_unit_str(key) result(val)
    integer, intent(in) ::  key
    ! character(BMI_MAX_TYPE_NAME) :: val
    character(:), allocatable :: val ! TODO: more efficient?
    if (key == 1) then
       val = "integer"
    else if (key == 2) then
       val = "real"
    else if (key == 3) then
       val = "double precision"
    else if (key == 4) then
       val = "logical"
    end if
  end function get_unit_str

end submodule bmi_wrf_hydro_nwm_smod
