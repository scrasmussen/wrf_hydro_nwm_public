# build the orchestrator static library
add_library(hydro_orchestrator STATIC
        config.F90
        io_manager.F90
        orchestrator.F90
)

add_dependencies(hydro_orchestrator
        hydro_netcdf_layer
        hydro_utils
        noahmp
        noahmp_hrldas_driver
)

target_link_libraries(hydro_orchestrator PRIVATE
        hydro_netcdf_layer
        hydro_utils
        noahmp_hrldas_driver
)
