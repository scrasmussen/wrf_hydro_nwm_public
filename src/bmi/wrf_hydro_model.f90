module wrf_hydro_model_mod
  type wrf_hydro_model
     integer :: stub
  end type wrf_hydro_model
  private :: init
contains
  subroutine init_from_defaults(model)
    type(wrf_hydro_model) :: model
    model%stub = -1
    print *, "stub model object initialized"
  end subroutine init_from_defaults
end module wrf_hydro_model_mod
