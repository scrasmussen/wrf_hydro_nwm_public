# Add MODFLOW libraries
# add external MODFLOW
add_library(modflow SHARED IMPORTED)
set(modflow_lib_dir "$ENV{MODFLOW_BMI}")
set(modflow_lib "${modflow_lib_dir}/libmf6.so")
set_target_properties(modflow PROPERTIES IMPORTED_LOCATION "${modflow_lib}")
# include_directories(${modflow_lib_dir}/libmf6.so.p) # remove this?

# add local MODFLOW BMI wrapper layer
add_library(bmi_modflow
  bmi_modflow_interface.f90
  bmi_modflow.f90)
target_link_libraries(bmi_modflow
  bmi_interface
  modflow)

# setup WRF-Hydro BMI Fortran drivers
add_executable(modflow_hello_world.exe
  wrf_hydro_nwm_modflow6_hello_world.f90)
target_link_libraries(modflow_hello_world.exe
  bmi_wrf_hydro_nwm
  bmi_modflow
  modflow
  ${CMAKE_THREAD_LIBS_INIT})

target_include_directories(bmi_modflow
  PUBLIC ${modflow_lib_dir}/libmf6.so.p/
  ${modflow_lib_dir}/../src/libmf6core.a.p/
  )
# target_include_directories(wrf_hydro_modflow.exe
#   PUBLIC ${modflow_lib_dir}/libmf6.so.p/)



# add_executable(wrf_hydro_modflow.exe
#   wrf_hydro_nwm_modflow6.f90)
# target_link_libraries(wrf_hydro_modflow.exe
#   bmi_wrf_hydro_nwm
#   bmi_modflow
#   modflow
#   ${CMAKE_THREAD_LIBS_INIT})



# target_link_libraries(wrf_hydro_modflow.exe
#   Threads::Threads)
# target_link_libraries(wrf_hydro_modflow.exe
#   /lib64/libpthread.so.0)
