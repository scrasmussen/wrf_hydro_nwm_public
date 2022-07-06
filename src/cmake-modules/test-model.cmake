cmake_minimum_required (VERSION 3.15)
message("Running test-mode.cmake")

# Download Croton, NY Test Files
set(croton_NY_tar "${CMAKE_BINARY_DIR}/croton_NY_training_example_v5.2.tar.gz")
if (NOT EXISTS ${croton_NY_tar})
  execute_process(COMMAND wget
    https://github.com/NCAR/wrf_hydro_nwm_public/releases/download/v5.2.0/croton_NY_training_example_v5.2.tar.gz
    COMMAND_ECHO STDOUT)
endif()

# Extract tarball
set(croton_NY_dir "${CMAKE_BINARY_DIR}/example_case")
if (NOT EXISTS ${croton_NY_dir})
  execute_process(COMMAND tar zxf ${croton_NY_tar}
    COMMAND_ECHO STDOUT)
endif()

# List of tables to copy to test directory
set(wrf_hydro_tables
  CHANPARM.TBL GENPARM.TBL HYDRO.TBL MPTABLE.TBL SOILPARM.TBL)
# List of test cases
set(croton_test_cases Gridded Gridded_no_lakes NWM Reach ReachLakes)
# Go into each test case directory and symlink to needed files
message("Symlinking in Croton test cases")
foreach(TEST_CASE ${croton_test_cases})
  execute_process(COMMAND
    ln -sf ${croton_NY_dir}/FORCING ${croton_NY_dir}/${TEST_CASE}/FORCING)
  execute_process(COMMAND
    ln -sf ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe ${croton_NY_dir}/${TEST_CASE}/wrf_hydro.exe)

  foreach(wrf_hydro_table ${wrf_hydro_tables})
    execute_process(COMMAND
      ln -sf ${CMAKE_BINARY_DIR}/Run/${wrf_hydro_table} ${croton_NY_dir}/${TEST_CASE}/${wrf_hydro_table})
  endforeach()
endforeach()

# Run WRF-Hydro in each test case
foreach(TEST_CASE ${croton_test_cases})
  if (DEFINED TEST_PARALLEL_MODEL)
    execute_process(COMMAND mpiexec ${croton_NY_dir}/${TEST_CASE}/wrf_hydro.exe
      WORKING_DIRECTORY ${croton_NY_dir}/${TEST_CASE}
      COMMAND_ECHO STDOUT)
  else()
    execute_process(COMMAND ${croton_NY_dir}/${TEST_CASE}/wrf_hydro.exe
      WORKING_DIRECTORY ${croton_NY_dir}/${TEST_CASE}
      COMMAND_ECHO STDOUT)
  endif()
endforeach()

message("Finished Running Tests")
