### Library Prerequisites
if (NOT TARGET esmf)
  find_package(ESMF MODULE REQUIRED)
endif (NOT TARGET esmf)

### Library Files
list(APPEND wrfhydro_nuopc_files
  wrfhydro_nuopc_utils.F90
  WRFHydro_NUOPC_Cap.F90
  WRFHydro_NUOPC_Gluecode.F90
  WRFHydro_NUOPC_Fields.F90
  WRFHydro_NUOPC_Flags.F90
  WRFHydro_ESMF_Extensions.F90
)

### Library Dependencies
list(APPEND wrfhydro_nuopc_deps
  hydro_routing
  hydro_mpp
  hydro_driver
  hydro_orchestrator
  hydro_noahmp
)

list(APPEND wrfhydro_nuopc_linklibs
  hydro_utils
  hydro_mpp
  hydro_debug_utils
  hydro_routing_overland
  hydro_routing_subsurface
  hydro_data_rec
  hydro_routing
  hydro_routing_diversions
  hydro_routing_reservoirs_levelpool
  hydro_routing_reservoirs_hybrid
  hydro_routing_reservoirs_rfc
  hydro_routing_reservoirs
  hydro_driver
  hydro_orchestrator
  hydro_noahmp
  hydro_netcdf_layer
  fortglob
  noahmp_util
  noahmp_phys
  noahmp_data
  hydro_noahmp_cpl
  snowcro
  crocus_surfex
)
if (WRF_HYDRO_NUDGING STREQUAL "1")
  list(APPEND wrfhydro_nuopc_linklibs
    hydro_nudging
  )
endif (WRF_HYDRO_NUDGING STREQUAL "1")

# if (MPAS_HYDRO)
#   set(hydro_nuopc_library "mpas_hydro")
#   list(APPEND wrfhydro_nuopc_files
#     mpas_hydro.F90)
# else()
 set(hydro_nuopc_library "wrfhydro_nuopc")
# endif()

add_library(wrfhydro_nuopc_obj
  OBJECT
  ${wrfhydro_nuopc_files})
add_dependencies(wrfhydro_nuopc_obj ${wrfhydro_nuopc_deps})
add_library(${hydro_nuopc_library}
  STATIC
  $<TARGET_OBJECTS:hydro_utils>
  $<TARGET_OBJECTS:hydro_mpp>
  $<TARGET_OBJECTS:hydro_debug_utils>
  $<TARGET_OBJECTS:hydro_routing_overland>
  $<TARGET_OBJECTS:hydro_routing_subsurface>
  $<TARGET_OBJECTS:hydro_data_rec>
  $<TARGET_OBJECTS:hydro_routing>
  $<TARGET_OBJECTS:hydro_routing_diversions>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_levelpool>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_hybrid>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_rfc>
  $<TARGET_OBJECTS:hydro_routing_reservoirs>
  $<TARGET_OBJECTS:hydro_driver>
  $<TARGET_OBJECTS:hydro_orchestrator>
  $<TARGET_OBJECTS:hydro_netcdf_layer>
  $<TARGET_OBJECTS:hydro_noahmp>
  $<TARGET_OBJECTS:hydro_noahmp_cpl>
  $<TARGET_OBJECTS:fortglob>
  $<TARGET_OBJECTS:wrfhydro_nuopc_obj>
  $<TARGET_OBJECTS:noahmp_data>
  $<TARGET_OBJECTS:noahmp_phys>
  $<TARGET_OBJECTS:noahmp_util>
  $<TARGET_OBJECTS:snowcro>
  $<TARGET_OBJECTS:crocus_surfex>
  )
target_link_libraries(${hydro_nuopc_library}
  PUBLIC
  esmf
  ${NETCDF_LIBRARIES}
  )


### Library Installation
install(TARGETS ${hydro_nuopc_library} ${wrfhydro_nuopc_linklibs}
  EXPORT  wrfhydro-config
  LIBRARY DESTINATION lib
)
if (MPAS_HYDRO)
  # set(CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_INSTALL_PREFIX})

  # # install(FILES ${mod_files}
  # install( ${CMAKE_Fortran_MODULE_DIRECTORY}/*
  #   DESTINATION ${CMAKE_INSTALL_PREFIX}
  # )
else()
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
    DESTINATION ${CMAKE_INSTALL_PREFIX}/WRFHYDRO
  )
endif()

install(EXPORT wrfhydro-config
  DESTINATION lib/cmake
)

message("----- wrfhydro_nuopc_deps = ${wrfhydro_nuopc_deps}")
message("----- wrfhydro_nuopc_linklibs = ${wrfhydro_nuopc_linklibs}")
message("----- hydro_nuopc_library = ${hydro_nuopc_library}")
# message(FATAL_ERROR "----- hydro_nuopc_library = ${hydro_nuopc_library}")
