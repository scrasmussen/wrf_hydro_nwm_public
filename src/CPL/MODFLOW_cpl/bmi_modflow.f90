submodule (bmi_modflow_mod) bmi_modflow_smod
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

  ! --------------------------------------------------------------------

  ! Get the name of the model.
  module procedure modflow_component_name
    use mf6bmi, only: bmi_get_component_name
    use iso_c_binding, only: c_char, c_null_char
    implicit none
    character(c_char) :: model_name(BMI_MAX_COMPONENT_NAME)
    character(len=256), allocatable, target :: f_name
    integer :: res, i
    integer, allocatable :: end(:)
    res = bmi_get_component_name(model_name)
    end = findloc(model_name, c_null_char)
    f_name = transfer(model_name(1:end(1)-1), f_name)
    name => f_name
    bmi_status = res
  end procedure ! modflow_component_name

  ! Perform startup tasks for the model.
  module procedure modflow_initialize
    use mf6bmi, only: bmi_initialize
    bmi_status = bmi_initialize()
  end procedure ! modflow_initialize

  ! Advance the model one time step.
  module procedure modflow_update
    use mf6bmi, only: bmi_update
    bmi_status = bmi_update()
  end procedure ! modflow_update

  ! Perform teardown tasks for the model.
  module procedure modflow_finalize
    use mf6bmi, only: bmi_finalize
    bmi_status = bmi_finalize()
  end procedure ! modflow_finalize

  ! --------------------------------------------------------------------
  !  The functions below this need to be implemented
  ! --------------------------------------------------------------------

  ! Get the grid identifier for the given variable.
  module procedure modflow_var_grid
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_grid

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable rank and the intrinsic function rank
  ! function get_grid_rank(grid) result(var_rank)
  !   integer :: var_rank
  !   var_rank = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_grid_rank

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable shape and the intrinsic function shape
  ! function get_grid_shape(grid) result(grid_shape)
  !   integer :: grid_shape
  !   grid_shape = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_grid_shape

  ! ! this function is need so that there isn't an object clash between the
  ! ! variable size and the intrinsic function size
  ! function get_var_size(grid) result(var_size)
  !   integer :: var_size
  !   var_size = STUB_I
  !   ! bmi_status = BMI_FAILURE
  ! end function get_var_size

  ! Get the grid type as a string.
  module procedure modflow_grid_type
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_type

  ! Set new values for an integer model variable.
  module procedure modflow_set_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_int

  ! Set new values for a real model variable.
  module procedure modflow_set_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_float

  ! Set new values for a double model variable.
  module procedure modflow_set_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_double

  ! Get a copy of values (flattened!) of the given integer variable.
  module procedure modflow_get_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_int

  ! Get a copy of values (flattened!) of the given real variable.
  module procedure modflow_get_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_float

  ! Get a copy of values (flattened!) of the given double variable.
  module procedure modflow_get_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_double

  ! Set integer values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_int

  ! Set real values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_float

  ! Set double values at particular (one-dimensional) indices.
  module procedure modflow_set_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_set_at_indices_double

  ! Get integer values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_int

  ! Get real values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_float

  ! Get double values at particular (one-dimensional) indices.
  module procedure modflow_get_at_indices_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_at_indices_double

  ! Advance the model until the given time.
  module procedure modflow_update_until
    bmi_status = BMI_FAILURE
  end procedure ! modflow_update_until

  ! Count a model's input variables.
  module procedure modflow_input_item_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_input_item_count

  ! Count a model's output variables.
  module procedure modflow_output_item_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_output_item_count

  !---------------------------------------------------------------------
  ! Should update how this is handeled in main_hrldas_driver
  !---------------------------------------------------------------------
  ! Start time of the model.
  module procedure modflow_start_time
    bmi_status = BMI_FAILURE
  end procedure ! modflow_start_time

  ! End time of the model.
  module procedure modflow_end_time
    bmi_status = BMI_FAILURE
  end procedure ! modflow_end_time

  ! Time step of the model.
  module procedure modflow_time_step
    time_step = 24.0
    bmi_status = BMI_FAILURE
  end procedure ! modflow_time_step

  ! Time units of the model.
  module procedure modflow_time_units
    units = "d"
    bmi_status = BMI_SUCCESS
  end procedure ! modflow_time_units

  ! Current time of the model.
  module procedure modflow_current_time
    bmi_status = BMI_FAILURE
  end procedure ! modflow_current_time

  ! Get size of the given variable, in bytes.
  module procedure modflow_var_nbytes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_nbytes

  ! Get number of dimensions of the computational grid.
  module procedure modflow_grid_rank
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_rank

  ! Get the dimensions of the computational grid.
  module procedure modflow_grid_shape
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_shape

  ! Get the data type of the given variable as a string.
  module procedure modflow_var_type
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_type

  ! Get the total number of elements in the computational grid.
  module procedure modflow_grid_size
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_size

  !---------------------------------------------------------------------
  ! Implemented but should be improved
  !---------------------------------------------------------------------

  ! List a model's input variables.
  module procedure modflow_input_var_names
    bmi_status = BMI_FAILURE
  end procedure ! modflow_input_var_names

  ! TODO: the following implementations are first takes for ldas, how do we add
  !       the other parts of the model?
  ! List a model's output variables.
  module procedure modflow_output_var_names
    bmi_status = BMI_FAILURE
  end procedure ! modflow_output_var_names

  module procedure modflow_var_units
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_units

  ! Get a reference to the given integer variable.
  module procedure modflow_get_ptr_int
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_int

  ! Get a reference to the given real variable.
  module procedure modflow_get_ptr_float
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_float

  ! Get a reference to the given double variable.
  module procedure modflow_get_ptr_double
    bmi_status = BMI_FAILURE
  end procedure ! modflow_get_ptr_double

  ! Describe where a variable is located: node, edge, or face.
  module procedure modflow_var_location
    bmi_status = BMI_FAILURE
  end procedure ! modflow_var_locatio

  ! Get distance between nodes of the computational grid.
  module procedure modflow_grid_spacing
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_spacing

  ! Get coordinates of the origin of the computational grid.
  module procedure modflow_grid_origin
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_origin

  ! Get the x-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_x
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_x

  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_y
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_y

  ! Get the z-coordinates of the nodes of a computational grid.
  module procedure modflow_grid_z
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_z

  ! Get the number of nodes in an unstructured grid.
  module procedure modflow_grid_node_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_node_count

  ! Get the number of edges in an unstructured grid.
  module procedure modflow_grid_edge_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  module procedure modflow_grid_face_count
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_count

  ! Get the edge-node connectivity.
  module procedure modflow_grid_edge_nodes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_edge_nodes

  ! Get the face-edge connectivity.
  module procedure modflow_grid_face_edges
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_edges

  ! Get the face-node connectivity.
  module procedure modflow_grid_face_nodes
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_face_nodes

  ! Get the number of nodes for each face.
  module procedure modflow_grid_nodes_per_face
    bmi_status = BMI_FAILURE
  end procedure ! modflow_grid_nodes_per_face

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
  ! module procedure print_model_info
  ! end procedure ! print_model_info

  ! Check the status and update bmi_status if necessary
  module procedure stat_check
    if (bmi_status == BMI_FAILURE) print *, "WHY??"
    if (status .ne. BMI_SUCCESS) then
       print *, " --- WARNING!! BMI_STATUS FAILED ---"
       bmi_status = BMI_FAILURE
    end if
  end procedure

  ! Get memory use per array element, in bytes.
  module procedure modflow_var_itemsize
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
  end procedure ! modflow_var_itemsize

end submodule bmi_modflow_smod
