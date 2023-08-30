add_library(noahmp_hrldas
  module_hrldas_netcdf_io.F
  module_NoahMP_hrldas_driver.F)

target_link_libraries(noahmp_hrldas
  noahmp_util
  noahmp_phys
  hydro_routing
  hydro_driver)
