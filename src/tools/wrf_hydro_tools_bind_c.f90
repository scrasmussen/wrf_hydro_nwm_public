module wrf_hydro_tools_bind_c
  use iso_c_binding, only: c_int
  implicit none

  interface
    ! Perform startup tasks for the model.
    module function get_domain_decomposition_c(np, x_np, y_np, nx, ny, &
       start_x, start_y, end_x, end_y) &
       result(res) &
       bind(c, name="get_domain_decomposition")
      integer(c_int), intent(in) :: np
      integer(c_int), intent(out) :: x_np, y_np, nx, ny
      integer(c_int), intent(out) :: start_x(np), start_y(np)
      integer(c_int), intent(out) :: end_x(np), end_y(np)
      integer(c_int) :: res
    end function get_domain_decomposition_c
 end interface

contains
  ! Get the y-coordinates of the nodes of a computational grid.
  module procedure get_domain_decomposition_c
    use wrf_hydro_tools, only: get_domain_decomposition
    call get_domain_decomposition(np, x_np, y_np, nx, ny, &
         start_x, start_y, end_x, end_y)
    res = 1
  end procedure ! get_domain_decomposition_c
end module wrf_hydro_tools_bind_c
