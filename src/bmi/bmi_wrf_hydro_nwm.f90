submodule (bmi_wrf_hydron_nwm) bmi_wrf_hydro_nwm_submod
contains
  module procedure wrf_hydro_component_name
    name => "WRF_HYDRO_FOOBAR"
    bmi_status = BMI_SUCCESS
  end procedure wrf_hydro_component_name
end submodule bmi_wrf_hydro_nwm_submod
