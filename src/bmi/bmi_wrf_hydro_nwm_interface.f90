module bmi_wrf_hydro_nwm_mod
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends(bmi), abstract :: bmi_wrf_hydro_nwm
     private
     ! type (wrf_hydro_model) :: model
   contains
     procedure :: get_component_name => wrf_hydro_component_name
     ! procedure :: get_input_item_count => wrf_hydro_input_item_count
     ! procedure :: get_output_item_count => wrf_hydro_output_item_count
     ! procedure :: get_input_var_names => wrf_hydro_input_var_names
     ! procedure :: get_output_var_names => wrf_hydro_output_var_names
     ! procedure :: initialize => wrf_hydro_initialize
     ! procedure :: finalize => wrf_hydro_finalize
     ! procedure :: get_start_time => wrf_hydro_start_time
     ! procedure :: get_end_time => wrf_hydro_end_time
     ! procedure :: get_current_time => wrf_hydro_current_time
     ! procedure :: get_time_step => wrf_hydro_time_step
     ! procedure :: get_time_units => wrf_hydro_time_units
     ! procedure :: update => wrf_hydro_update
     ! procedure :: update_until => wrf_hydro_update_until
     ! procedure :: get_var_grid => wrf_hydro_var_grid
     ! procedure :: get_grid_type => wrf_hydro_grid_type
     ! procedure :: get_grid_rank => wrf_hydro_grid_rank
     ! procedure :: get_grid_shape => wrf_hydro_grid_shape
     ! procedure :: get_grid_size => wrf_hydro_grid_size
     ! procedure :: get_grid_spacing => wrf_hydro_grid_spacing
     ! procedure :: get_grid_origin => wrf_hydro_grid_origin
     ! procedure :: get_grid_x => wrf_hydro_grid_x
     ! procedure :: get_grid_y => wrf_hydro_grid_y
     ! procedure :: get_grid_z => wrf_hydro_grid_z
     ! procedure :: get_grid_node_count => wrf_hydro_grid_node_count
     ! procedure :: get_grid_edge_count => wrf_hydro_grid_edge_count
     ! procedure :: get_grid_face_count => wrf_hydro_grid_face_count
     ! procedure :: get_grid_edge_nodes => wrf_hydro_grid_edge_nodes
     ! procedure :: get_grid_face_edges => wrf_hydro_grid_face_edges
     ! procedure :: get_grid_face_nodes => wrf_hydro_grid_face_nodes
     ! procedure :: get_grid_nodes_per_face => wrf_hydro_grid_nodes_per_face
     ! procedure :: get_var_type => wrf_hydro_var_type
     ! procedure :: get_var_units => wrf_hydro_var_units
     ! procedure :: get_var_itemsize => wrf_hydro_var_itemsize
     ! procedure :: get_var_nbytes => wrf_hydro_var_nbytes
     ! procedure :: get_var_location => wrf_hydro_var_location
     ! procedure :: get_value_int => wrf_hydro_get_int
     ! procedure :: get_value_float => wrf_hydro_get_float
     ! procedure :: get_value_double => wrf_hydro_get_double
     ! generic :: get_value => &
     !      get_value_int, &
     !      get_value_float, &
     !      get_value_double
     ! procedure :: get_value_ptr_int => wrf_hydro_get_ptr_int
     ! procedure :: get_value_ptr_float => wrf_hydro_get_ptr_float
     ! procedure :: get_value_ptr_double => wrf_hydro_get_ptr_double
     ! generic :: get_value_ptr => &
     !      get_value_ptr_int, &
     !      get_value_ptr_float, &
     !      get_value_ptr_double
     ! procedure :: get_value_at_indices_int => wrf_hydro_get_at_indices_int
     ! procedure :: get_value_at_indices_float => wrf_hydro_get_at_indices_float
     ! procedure :: get_value_at_indices_double => wrf_hydro_get_at_indices_double
     ! generic :: get_value_at_indices => &
     !      get_value_at_indices_int, &
     !      get_value_at_indices_float, &
     !      get_value_at_indices_double
     ! procedure :: set_value_int => wrf_hydro_set_int
     ! procedure :: set_value_float => wrf_hydro_set_float
     ! procedure :: set_value_double => wrf_hydro_set_double
     ! generic :: set_value => &
     !      set_value_int, &
     !      set_value_float, &
     !      set_value_double
     ! procedure :: set_value_at_indices_int => wrf_hydro_set_at_indices_int
     ! procedure :: set_value_at_indices_float => wrf_hydro_set_at_indices_float
     ! procedure :: set_value_at_indices_double => wrf_hydro_set_at_indices_double
     ! generic :: set_value_at_indices => &
     !      set_value_at_indices_int, &
     !      set_value_at_indices_float, &
     !      set_value_at_indices_double
     ! procedure :: print_model_info
  end type bmi_wrf_hydro_nwm

  interface
     ! Get the name of the model.
     module function wrf_hydro_component_name(this, name) result(bmi_status)
       class(bmi_wrf_hydro_nwm), intent(in) :: this
       character(len=*), pointer, intent(out) :: name
       integer :: bmi_status
     end function wrf_hydro_component_name
  end interface
end module bmi_wrf_hydro_nwm_mod
