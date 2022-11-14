# build the orchestrator static library
add_library(hydro_orchestrator STATIC
        config.f90
        io_manager.f90
        orchestrator.f90
        )

add_dependencies(hydro_orchestrator hydro_netcdf_layer)
add_dependencies(hydro_orchestrator hydro_utils)
add_dependencies(hydro_orchestrator noahmp_refactor_noahmp)

target_link_libraries(hydro_orchestrator PUBLIC hydro_netcdf_layer)
target_link_libraries(hydro_orchestrator PUBLIC hydro_utils)
