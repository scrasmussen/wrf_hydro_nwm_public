submodule (bmi_wrf_hydro_nwm_mod) bmi_wrf_hydro_nwm_smod
  implicit none
contains

  module procedure wrf_hydro_component_name
    character(len=128), target :: model_name
    model_name = "WRF-HYDRO v5.3.x"
    name => model_name
    bmi_status = BMI_SUCCESS
  end procedure !wrf_hydro_component_name

end submodule bmi_wrf_hydro_nwm_smod
