program reservoir_unit_tests_driver
  use module_reservoir_unit_tests
  implicit none
  logical :: rv1 = .false.
  logical :: rv2 = .false.
  logical :: rv3 = .false.
  logical :: rv4 = .false.
  logical :: rv5 = .false.
  logical :: rv6 = .false.
  logical :: rv7 = .false.
  logical :: rv8 = .false.
  logical :: rv9 = .false.
  logical :: rv10 = .false.
  logical :: rv11 = .false.
  logical :: rv12 = .false.
  logical :: rv13 = .false.
  logical :: rv14 = .false.
  logical :: rv15 = .false.
  logical :: rv16 = .false.
  logical :: rv17 = .false.

  rv1 = test_levelpool()

  rv2 = test_levelpool_overflow_max_height()

  rv3 = test_persistence_levelpool_hybrid_usgs()

  rv4 = test_persistence_levelpool_hybrid_usace()

  rv5 = test_rfc_forecasts_object()

  rv6 = test_rfc_forecasts_time_series_object()

  rv7 = test_rfc_forecasts_levelpool_fallback()

  rv8 = test_rfc_forecasts_time_series_output_with_lookback_and_offset()

  rv9 = test_rfc_forecasts_time_series_output_with_negative_data()

  rv10 = test_rfc_forecasts_time_series_output_all_synthetic_data()

  rv11 = test_rfc_forecasts_data_exceeding_max_range()

  rv12 = test_rfc_forecasts_with_offset_for_extended_AnA()

  rv13 = test_persistence_levelpool_hybrid_usace_overtop()

  rv14 = test_persistence_levelpool_hybrid_usace_no_timeslice_available()

  rv15 = test_ak_rfc_forecasts_pass_through_fallback()

  rv16 = test_ak_rfc_forecasts_time_series_with_lookback_and_offset()

  rv17 = test_rfc_forecasts_over_max_water_elevation()

  if (rv1 .and. rv2 .and. rv3 .and. rv4 .and. rv5 .and. rv6 .and. rv7 .and. rv8 .and. rv9 .and. &
       rv10 .and. rv11 .and. rv12 .and. rv13 .and. rv14 .and. rv15 .and. rv16 .and. rv17) then
     print *, "========================================================================"
     print *, 'All Reservoir Tests Passed'
     print *, "========================================================================"

  else
     print *, "========================================================================"
     print *, 'Not All Reservoir Tests Passed'
     print *, "========================================================================"
  end if


end program
