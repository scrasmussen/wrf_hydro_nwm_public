# build the various sup-projects
add_subdirectory("MPP")
add_subdirectory("utils")
add_subdirectory("IO")
add_subdirectory("OrchestratorLayer")
add_subdirectory("Debug_Utilities")
add_subdirectory("Routing/Overland")
add_subdirectory("Routing/Subsurface")
#add_subdirectory("Routing/Groundwater")
add_subdirectory("Routing/Reservoirs")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")

# add Basic Module Interface (BMI)
add_subdirectory("CPL/BMI_cpl")

if (WRF_HYDRO_NUDGING STREQUAL "1")
    add_subdirectory("nudging")
    add_dependencies(hydro_driver hydro_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")

if (WRF_HYDRO_NUOPC STREQUAL "1")
        add_subdirectory("CPL/NUOPC_cpl")
endif (WRF_HYDRO_NUOPC STREQUAL "1")

# add module dependencies
add_dependencies(hydro_debug_utils hydro_mpp)
add_dependencies(hydro_utils hydro_mpp)

add_dependencies(hydro_orchestrator hydro_netcdf_layer)

add_dependencies(hydro_routing hydro_mpp)
add_dependencies(hydro_routing hydro_routing_overland)
add_dependencies(hydro_routing hydro_routing_subsurface)
add_dependencies(hydro_routing hydro_routing_reservoirs)
add_dependencies(hydro_routing hydro_routing_reservoirs_levelpool)
add_dependencies(hydro_routing hydro_routing_reservoirs_hybrid)
add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_levelpool)
#add_dependencies(hydro_routing hydro_routing_groundwater)
#add_dependencies(hydro_routing hydro_routing_groundwater_bucket)
#add_dependencies(hydro_routing hydro_routing_groundwater_nhd)
#add_dependencies(hydro_routing hydro_routing_groundwater_simple)
add_dependencies(hydro_routing hydro_utils)

add_dependencies(hydro_routing_overland hydro_mpp)

#add_dependencies(hydro_routing_groundwater hydro_mpp)
#add_dependencies(hydro_routing_groundwater_bucket hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater_bucket)
#add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater_bucket)

add_dependencies(hydro_driver hydro_routing)
add_dependencies(hydro_driver hydro_debug_utils)

add_dependencies(hydro_data_rec hydro_routing_overland)
add_dependencies(hydro_data_rec hydro_routing_subsurface)
#add_dependencies(hydro_data_rec hydro_routing_groundwater)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_bucket)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_simple)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_nhd)
add_dependencies(hydro_data_rec hydro_routing_reservoirs)

add_library(wrfhydro_lib INTERFACE)


if (HYDRO_LSM MATCHES "NoahMP")
    message ("-- Building NoahMP LSM")
    add_subdirectory("Land_models/NoahMP")

    add_subdirectory("CPL/NoahMP_cpl")
    add_dependencies(hydro_noahmp_cpl hydro_routing)
    add_dependencies(hydro_noahmp_cpl hydro_mpp )
    add_dependencies(hydro_noahmp_cpl hydro_driver )

    add_library(module_NoahMP_hrldas
        Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.F
        Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F)

    add_executable(wrfhydro.exe
        Land_models/NoahMP/IO_code/main_hrldas_driver.F)

    target_include_directories(wrfhydro.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

    target_link_libraries(wrfhydro_lib INTERFACE
        hydro_utils
        hydro_mpp
        hydro_debug_utils
        hydro_routing_overland
        hydro_routing_subsurface
        # hydro_routing_groundwater
        # hydro_routing_groundwater_bucket
        # hydro_routing_groundwater_nhd
        # hydro_routing_groundwater_simple
        hydro_data_rec
        hydro_routing
        hydro_routing_reservoirs_levelpool
        hydro_routing_reservoirs_hybrid
        hydro_routing_reservoirs_rfc
        hydro_routing_reservoirs
        hydro_driver
        noahmp_util
        noahmp_phys
        noahmp_data
        hydro_noahmp_cpl
        ${NETCDF_LIBRARIES})

    add_dependencies(wrfhydro.exe wrf_hydro_bmi.exe)

    target_link_libraries(module_NoahMP_hrldas
        wrfhydro_lib)

    target_link_libraries(wrfhydro.exe
        wrfhydro_lib
        module_NoahMP_hrldas)

    if (WRF_HYDRO_NUDGING STREQUAL "1")
        target_link_libraries(wrfhydro.exe hydro_nudging)
        add_dependencies(wrfhydro.exe hydro_nudging)
    endif (WRF_HYDRO_NUDGING STREQUAL "1")

    add_custom_command(TARGET wrfhydro.exe POST_BUILD
        # Run can have directories after running CTest, running more specific rm commands
        # COMMAND rm -rf ${CMAKE_BINARY_DIR}/Run/example_case
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP.exe
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*.exe
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*.TBL
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/hydro.namelist
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/namelist.hrldas
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/2011*
        COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/diag_hydro.*

        COMMAND cp ${PROJECT_BINARY_DIR}/src/wrfhydro.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP.exe
        COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/NoahMP/* ${CMAKE_BINARY_DIR}/Run
        COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
        COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
        COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
        COMMAND cp ${PROJECT_SOURCE_DIR}/src/Land_models/NoahMP/run/*.TBL ${CMAKE_BINARY_DIR}/Run
        COMMAND ln -s wrf_hydro_NoahMP.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
        COMMAND rm ${PROJECT_BINARY_DIR}/src/wrfhydro.exe
	COMMAND cp ${PROJECT_BINARY_DIR}/src/CPL/BMI_cpl/wrf_hydro_bmi.exe ${CMAKE_BINARY_DIR}/Run/
    )

elseif(HYDRO_LSM MATCHES "Noah")
    message ("-- Building Noah LSM")
    add_subdirectory("Land_models/Noah")
    add_subdirectory("CPL/Noah_cpl")

        add_dependencies(hydro_noah_cpl hydro_routing)
        add_dependencies(hydro_noah_cpl hydro_mpp )
        add_dependencies(hydro_noah_cpl hydro_driver )

	add_library(module_Noah_hrldas
            Land_models/Noah/IO_code/module_hrldas_netcdf_io.F)

	add_executable(wrfhydro.exe
            Land_models/Noah/IO_code/Noah_hrldas_driver.F)

        target_include_directories(wrfhydro.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

	target_link_libraries(wrfhydro_lib INTERFACE
            hydro_utils
            hydro_mpp
            hydro_debug_utils
            hydro_routing_overland
            hydro_routing_subsurface
            # hydro_routing_groundwater
            # hydro_routing_groundwater_bucket
            # hydro_routing_groundwater_nhd
            # hydro_routing_groundwater_simple
            hydro_data_rec
            hydro_routing
            hydro_driver
            hydro_routing_reservoirs_levelpool
            hydro_routing_reservoirs_hybrid
            hydro_routing_reservoirs_rfc
            hydro_routing_reservoirs
            noah_util
            noah
            hydro_noah_cpl
            ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

	add_dependencies(wrfhydro.exe wrf_hydro_bmi.exe)

	target_link_libraries(module_Noah_hrldas
	    wrfhydro_lib)

	target_link_libraries(wrfhydro.exe
  	    wrfhydro_lib
	    module_Noah_hrldas)

        if (WRF_HYDRO_NUDGING STREQUAL "1")
            target_link_libraries(wrfhydro.exe hydro_nudging)
            add_dependencies(wrfhydro.exe hydro_nudging)
        endif (WRF_HYDRO_NUDGING STREQUAL "1")

        add_custom_command(TARGET wrfhydro.exe POST_BUILD
            COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*
            COMMAND cp ${PROJECT_BINARY_DIR}/src/wrfhydro.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro_Noah.exe
            COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/Noah/* ${CMAKE_BINARY_DIR}/Run
            COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
            COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
            COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
            COMMAND cp ${PROJECT_SOURCE_DIR}/src/Land_models/Noah/Run/*.TBL ${CMAKE_BINARY_DIR}/Run
            COMMAND ln -s wrf_hydro_Noah.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
            COMMAND rm ${PROJECT_BINARY_DIR}/src/wrfhydro.exe
        )

else()
    message ("Unknown land surface model:" ${HYDRO_LSM} )
endif (HYDRO_LSM MATCHES "NoahMP")
