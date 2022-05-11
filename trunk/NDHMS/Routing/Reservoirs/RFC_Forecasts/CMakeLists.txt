cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs_rfc STATIC
	module_rfc_forecasts.F
	module_rfc_forecasts_state.F
	module_rfc_forecasts_properties.F
	module_rfc_forecasts_tests.F
)

target_link_libraries(hydro_routing_reservoirs_rfc
  hydro_routing_reservoirs
  hydro_routing_reservoirs_levelpool)
