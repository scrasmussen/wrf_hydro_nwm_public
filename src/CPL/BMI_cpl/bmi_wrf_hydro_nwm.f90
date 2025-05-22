submodule (bmi_wrf_hydro_nwm_mod) bmi_wrf_hydro_nwm_smod
  implicit none
  integer, parameter :: STUB_I = -1
  integer, dimension(1), parameter :: STUB_1D_I = [-1]
  double precision, parameter :: STUB_D = -1.0
  double precision, dimension(1), parameter :: STUB_1D_D = [-1.0]
  character(len=11), parameter :: STUB_C = "STUB RESULT"

  interface set_var_at_indices
     module procedure :: set_2d_var_at_indices_int
     module procedure :: set_3d_var_at_indices_int
     module procedure :: set_2d_var_at_indices_float
     module procedure :: set_3d_var_at_indices_float
     module procedure :: set_2d_var_at_indices_double
     module procedure :: set_3d_var_at_indices_double
  end interface set_var_at_indices

  interface index_to_multidim
     module procedure :: index_to_multidim_2d
     module procedure :: index_to_multidim_3d
  end interface index_to_multidim
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

  ! this function is need so that there isn't an object clash between the
  ! variable rank and the intrinsic function rank
  function get_grid_rank(grid) result(var_rank)
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP, GLACT
    integer, intent(in) :: grid
    integer :: var_rank
    select case(grid)
    case(1) ! "IVGTYP"
       var_rank = rank(IVGTYP)
    case(2) ! "ISLTYP"
       var_rank = rank(ISLTYP)
    case(4) ! GLACT
       var_rank = rank(GLACT)
    case default
       var_rank = -1
       print *, "WARNING: variable ", grid, " not found, rank set to -1"
    end select
  end function get_grid_rank

  ! Get number of dimensions of the computational grid.
  module procedure wrf_hydro_grid_rank
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    bmi_status = BMI_SUCCESS
    rank = get_grid_rank(grid)
    if (rank == -1) bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_rank

  ! this function is need so that there isn't an object clash between the
  ! variable shape and the intrinsic function shape
  function get_grid_shape(grid) result(grid_shape)
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP, GLACT
    integer, intent(in) :: grid
    integer, dimension(:), allocatable :: grid_shape
    select case(grid)
    case(1) ! "IVGTYP"
       grid_shape = shape(IVGTYP)
    case(2) ! "ISLTYP"
       grid_shape = shape(ISLTYP)
    case(4) ! GLACT
       grid_shape = shape(GLACT)
    case default
       grid_shape = [0]
       print *, "WARNING: grid", grid, " was not found"
    end select
  end function get_grid_shape

  ! Get the dimensions of the computational grid.
  module procedure wrf_hydro_grid_shape
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    bmi_status = BMI_SUCCESS
    shape = get_grid_shape(grid)
    ! convert from 'xy' indexing to 'ij' indexing
    shape = cshift(shape, 1)
    if (size(shape) == 3) then
       shape(1:2) = cshift(shape(1:2), 1)
    end if
    print *, "SHAPE HERE IS ", shape, "|", size(shape)
    if (shape(1) == 0) bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_grid_shape

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
    character(BMI_MAX_TYPE_NAME) :: var_type
    res = this%get_var_grid(name, grid)
    res = this%get_grid_type(grid, var_type)
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
    case("GLACT")
       grid = 4
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
    case(1) ! "IVGTYP"
       var_size = size(IVGTYP)
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

  ! Get the grid type as a string.
  module procedure wrf_hydro_grid_type
    bmi_status = BMI_SUCCESS
    ! options are:
    !   - "integer"
    !   - "real"
    !   - "double precision"
    !   - "logical"
    select case(grid)
    case(1) ! "IVGTYP"
       type = "integer"
    case(2) ! "ISLTYP"
       type = "integer"
    case(4) ! GLACT
       type = "real"
    case default
       type = ""
       print *, "WARNING: variable ", grid, " not found"
       bmi_status = BMI_FAILURE
    end select
  end procedure ! wrf_hydro_grid_type

  ! Get real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_float
    use module_NoahMP_hrldas_driver, only : GLACT
    real, allocatable ::  pack_data(:), ind_data(:)
    integer :: n, i
    logical :: unpack
    unpack = .true.
    bmi_status = BMI_SUCCESS

    ! TODO: packing is easy but inefficient way of doing this
    print *, "WARNING: PACKING IS INEFFICIENT"
    select case(name)
    case("GLACT")
       pack_data = pack(GLACT, .true.)
    case default
       unpack = .false.
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select

    ! if data found, unpack data
    if (unpack .eqv. .true.) then
       n = size(inds)
       allocate(ind_data(n))
       do i=1,n
          ind_data(i) = pack_data(inds(i))
       end do
       dest = ind_data
    end if
  end procedure ! wrf_hydro_get_at_indices_float

  ! Set new values for an integer model variable.
  module procedure wrf_hydro_set_int
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    bmi_status = BMI_SUCCESS
    select case(name)
    case("IVGTYP")
       IVGTYP = reshape(src, shape(IVGTYP))
    case("ISLTYP")
       ISLTYP = reshape(src, shape(ISLTYP))
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_int

  ! Set new values for a real model variable.
  module procedure wrf_hydro_set_float
    use module_NoahMP_hrldas_driver, only : GLACT
    bmi_status = BMI_SUCCESS
    select case(name)
    case("GLACT")
       GLACT = reshape(src, shape(GLACT))
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_float

  ! Set new values for a double model variable.
  module procedure wrf_hydro_set_double
    bmi_status = BMI_SUCCESS
    select case(name)
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_double



  !---------------------------------------------------------------------
  ! STUBS: Section consists of stubs to allow building and testing.
  !        Move above when implemented.
  !---------------------------------------------------------------------

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure wrf_hydro_get_int
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    integer :: res, grid, size
    integer, allocatable ::  var_data(:)
    bmi_status = BMI_SUCCESS
    res = this%get_var_grid(name, grid)
    select case(grid)
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
       bmi_status = BMI_FAILURE
    end select
  end procedure ! wrf_hydro_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure wrf_hydro_get_float
    use module_NoahMP_hrldas_driver, only : GLACT
    integer :: res, grid, size
    real, allocatable ::  var_data(:)
    bmi_status = BMI_SUCCESS
    res = this%get_var_grid(name, grid)
    select case(grid)
    case(4) ! GLACT
       dest = pack(GLACT, .true.)
    case default
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure wrf_hydro_get_double
    integer :: res, grid, size
    double precision, allocatable ::  var_data(:)
    bmi_status = BMI_SUCCESS
    res = this%get_var_grid(name, grid)
    select case(grid)
    ! case(X) ! future_var
    !    dest = pack(future_var, .true.)
    case default
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_get_double

  ! Get a reference to the given real variable.
  module procedure wrf_hydro_get_ptr_float
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    type (c_ptr) :: src
    ! WRF-Hydro internal variables are not targets and currently cannot
    ! support this function
    dest_ptr => NULL()
    bmi_status = BMI_FAILURE
    select case(name)
    case("Fortran indexing starts at 1")
       dest_ptr => NULL()
    case("IVGTYP")
       ! src = c_loc(IVGTYP) ! VARIABLE TO A TARGET
       ! call c_f_pointer(src, dest_ptr, [n_elements])
    case("ISLTYP")
    case default
       dest_ptr => NULL()
       bmi_status = BMI_FAILURE
       print *, "WARNING: variable ptr ", trim(name), " not found"
    end select

    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_float

  ! Get integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_int
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    integer, allocatable ::  pack_data(:), ind_data(:)
    integer :: n, i
    logical :: unpack
    unpack = .true.
    bmi_status = BMI_SUCCESS

    ! TODO: packing is easy but inefficient way of doing this
    print *, "WARNING: PACKING IS INEFFICIENT"
    select case(name)
    case("IVGTYP")
       pack_data = pack(IVGTYP, .true.)
    case("ISLTYP")
       pack_data = pack(ISLTYP, .true.)
    case default
       unpack = .false.
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select

    ! if data found, unpack data
    if (unpack .eqv. .true.) then
       n = size(inds)
       allocate(ind_data(n))
       do i=1,n
          ind_data(i) = pack_data(inds(i))
       end do
       dest = ind_data
    end if
  end procedure ! wrf_hydro_get_at_indices_int

  ! Set integer values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_int
    use module_NoahMP_hrldas_driver, only : IVGTYP, ISLTYP
    integer, allocatable :: var_shape(:)
    bmi_status = BMI_SUCCESS
    select case(name)
    case("IVGTYP")
       call set_var_at_indices(IVGTYP, inds, src)
    case("ISLTYP")
       var_shape = shape(ISLTYP)
       ! ISLTYP =
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_float
    use module_NoahMP_hrldas_driver, only : GLACT
    real, allocatable :: var_shape(:)
    bmi_status = BMI_SUCCESS
    select case(name)
    case("GLACT")
       call set_var_at_indices(GLACT, inds, src)
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_set_at_indices_double
    use module_NoahMP_hrldas_driver, only : GLACT
    double precision, allocatable :: var_shape(:)
    bmi_status = BMI_SUCCESS
    select case(name)
    ! case("future_var")
    !    call set_var_at_indices(future_var, inds, src)
    case default
       bmi_status = BMI_FAILURE
       print *, "WARNING: ", trim(name), " data not found"
    end select
  end procedure ! wrf_hydro_set_at_indices_double

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
  end procedure ! wrf_hydro_var_locatio

  ! Get a reference to the given integer variable.
  module procedure wrf_hydro_get_ptr_int
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_int

  ! Get a reference to the given double variable.
  module procedure wrf_hydro_get_ptr_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_ptr_double

  ! Get double values at particular (one-dimensional) indices.
  module procedure wrf_hydro_get_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! wrf_hydro_get_at_indices_double

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

  ! --- following set of subroutines handles the conversion of 1d flat index
  !     to 2d or 3d dimensions ---
  subroutine index_to_multidim_3d(index, nx, ny, i, j, k)
    integer, intent(in) :: index, nx, ny
    integer, intent(out) :: i,j,k
    i = modulo(index-1, nx)
    j = modulo((index-1) / nx, ny)
    k = (index-1)  / (ny * nx)
    i = i + 1
    j = j + 1
    k = k + 1
  end subroutine index_to_multidim_3d

  subroutine index_to_multidim_2d(index, nx, i, j)
    integer, intent(in) :: index, nx
    integer, intent(out) :: i,j
    i = modulo(index-1, nx)
    j = (index-1) / nx
    i = i + 1
    j = j + 1
  end subroutine index_to_multidim_2d

  ! --- following set of subroutines set the indices of a 2d or 3d
  !     variable for its given type ---
  subroutine set_2d_var_at_indices_int(var, inds, src)
    integer, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_int

  subroutine set_3d_var_at_indices_int(var, inds, src)
    integer, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_int

  subroutine set_2d_var_at_indices_float(var, inds, src)
    real, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_float

  subroutine set_3d_var_at_indices_float(var, inds, src)
    real, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_float

  subroutine set_2d_var_at_indices_double(var, inds, src)
    double precision, intent(inout) :: var(:,:)
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: index, ii, i, j, var_shape(2), nx
    var_shape = shape(var)
    nx = var_shape(1)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, i, j)
       var(i,j) = src(ii)
    end do
  end subroutine set_2d_var_at_indices_double

  subroutine set_3d_var_at_indices_double(var, inds, src)
    double precision, intent(inout) :: var(:,:,:)
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: index, ii, i, j, k, var_shape(3), nx, ny
    var_shape = shape(var)
    nx = var_shape(1)
    ny = var_shape(2)
    do ii=1,size(inds)
       call index_to_multidim(inds(ii), nx, ny, i, j, k)
       var(i,j,k) = src(ii)
    end do
  end subroutine set_3d_var_at_indices_double

  ! Model introspection.
  module procedure print_model_info
  end procedure ! print_model_info

  ! Check the status and update bmi_status if necessary
  module procedure stat_check
    if (bmi_status == BMI_FAILURE) print *, "WHY??"
    if (status .ne. BMI_SUCCESS) then
       print *, " --- WARNING!! BMI_STATUS FAILED ---"
       bmi_status = BMI_FAILURE
    end if
  end procedure

end submodule bmi_wrf_hydro_nwm_smod
