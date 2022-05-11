cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs STATIC
	module_reservoir.F
	module_reservoir_read_timeslice_data.F
	module_reservoir_read_rfc_time_series_data.F
	module_reservoir_utilities.F
)

target_link_libraries(hydro_routing_reservoirs
  hydro_utils)

add_library(reservoir_unit_tests STATIC
  	module_reservoir_unit_tests.F
)

target_link_libraries(reservoir_unit_tests
  hydro_utils
  hydro_levelpool_tests
  hydro_routing_reservoirs_levelpool
  hydro_routing_reservoirs_hybrid
  hydro_routing_reservoirs_rfc)

add_subdirectory("Level_Pool")
add_subdirectory("Persistence_Level_Pool_Hybrid")
add_subdirectory("RFC_Forecasts")
