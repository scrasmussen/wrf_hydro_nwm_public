# WRF-Hydro and ParFlow ESMF NUOPC Coupling

This code allows WRF-Hydro to couple with the Earth System Modeling Framework ([ESMF](https://github.com/esmf-org/esmf/tree/develop)).
The following build allows coupling between WRF-Hydro and [ParFlow](https://parflow.org/) using [ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX), which is a part of ESMF.
ESMX is a tool to help build and run coupled models.

## Install ESMF
``` bash
$ wget https://github.com/esmf-org/esmf/archive/refs/tags/v8.5.0.tar.gz
$ tar zxf v8.5.0.tar.gz
$ cd esmf-8.5.0
$ export ESMF_DIR=$(pwd)
$ export ESMF_INSTALL_PREFIX=/path/to/install/esmf/8.5.0
$ export ESMF_F90COMPILER=mpif90
$ export ESMF_F90LINKER=mpif90
$ make -j 4
$ make install
```

The following is an example of an `esmf/8.5.0.lua` module file.
``` lua
local base = "/path/to/install/esmf/8.5.0/"
local bin = pathJoin(base, "bin/binO/Linux.gfortran.64.mpiuni.default")
local lib = pathJoin(base, "lib/libO/Linux.gfortran.64.mpiuni.default")
local include = pathJoin(base, "include")
local mod = pathJoin(base, "mod/modO/Linux.gfortran.64.mpiuni.default")
local mk_file = pathJoin(lib,"esmf.mk")
local esmx = pathJoin(base,"include/ESMX/")

prepend_path("PATH", bin)
prepend_path("PATH", mod)
prepend_path("PATH", include)
prepend_path("LD_LIBRARY_PATH", lib)
setenv("ESMFMKFILE",mk_file)
setenv("ESMF_LIBRARY_LOCATION",lib)
setenv("ESMF_ESMXDIR", esmx)
setenv("ESMF_F90COMPILER", "mpif90")
setenv("ESMF_F90LINKER", "mpif90")
```
**NOTE**: Delete the last few lines from `ESMFMKFILE`. There maybe a better way than deleting, but at some point that file is grepped for environment variables and the commented out user variables break that functionality.


## Install ParFlow Optional Prerequisites
ParFlow can be installed with other packages.
The following shows how to install the [Silo](https://github.com/LLNL/Silo) and [Hypre](https://computing.llnl.gov/projects/hypre-scalable-linear-solvers-multigrid-methods) libraries, thich have been used in this coupling.

### Silo Option
``` bash
$ wget https://github.com/LLNL/Silo/archive/refs/tags/4.10.2.tar.gz
$ tar zxf 4.10.2
$ cd Silo-4.10.2
$ ./configure --enable-fortran --prefix=/path/to/install/silo/4.10.2
$ make -j 4
$ make install
```

### Hypre Option
``` bash
$ wget https://github.com/hypre-space/hypre/archive/refs/tags/v2.29.0.tar.gz
$ tar zxf v2.29.0.tar.gz
$ cd hypre-2.29.0/src
$ ./configure --enable-fortran --prefix=/path/to/install/hypre/2.29.0
$ make -j 4
$ make install
```

### Install PyYAML
Use [conda](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html) or [pip](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/) to manage your Python library environment. The following example uses conda.
```
$ conda create --name conda-yaml
$ conda activate /path/to/install/esmx-req
$ conda install PyYAML
```
NOTE: in future `conda activate /path/to/install/esmx-req` will need to be run to load the libraries.



## Build with ESMX
This uses the shell script `ESMX_Builder`, the instructions below can be modified if needed using the [build documentation](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX#esmx_builder) and [run documentation](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX#esmx_builder).
`ESMX_Builder` takes a YAML file, in most cases called `esmxBuild.yaml`, as an argument that instructs it how to build the coupled models.
The following YAML file is for coupling WRF-Hydro and Parflow.
``` yaml
components:
  parflow:
    git_repository: git@github.com:esmf-org/parflow.git
    git_tag: feature/NUOPC/ESMFAPP-6
    git_dir: parflow
    source_dir: parflow
    fort_module: parflow_nuopc.mod
    libraries: parflow_nuopc pfsimulator amps pfkinsol cjson pfclm
    build_args: -DPARFLOW_ENABLE_NUOPC=ON -DPARFLOW_AMPS_LAYER=mpi1 -DPARFLOW_HAVE_CLM=ON -DPARFLOW_ENABLE_HYPRE=ON -DPARFLOW_ENABLE_SILO=ON -DPARFLOW_ENABLE_NETCDF=ON

  wrfhydro:
    git_repository: git@github.com:scrasmussen/wrf_hydro_nwm_public.git
    git_tag: enhancement/nuopc-cap-rebase-esmf
    git_dir: wrfhydro
    source_dir: wrfhydro
    include_dir: WRFHYDRO/mods
    fort_module: wrfhydro_nuopc.mod
    libraries: wrfhydro_nuopc hydro_driver hydro_orchestrator hydro_routing hydro_utils hydro_mpp hydro_debug_utils hydro_routing_overland hydro_routing_subsurface hydro_data_rec hydro_routing_reservoirs_levelpool hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_rfc hydro_routing_reservoirs hydro_netcdf_layer
    build_args: -DWRF_HYDRO=1 -DWRF_HYDRO_NUOPC=1
```

To run the `ESMX_Builder` script, run the following Bash script or modify for command line use.
``` bash
#!/bin/bash
set -e -x

# set environment variables
SILO_ROOT=/path/to/silo/4.10.2
HYPRE_ROOT=/path/to/hypre/2.29.0

# run build script
ESMX_Builder esmxBuild.yaml
```

## Run with ESMX
After building the `esmx_app` executable will be available under the `bin` directory in the specific installation path or the default `install` directory.
The coupled application is run with `$ esmx_app esmxRun.yaml`.
The following is an example of the `esmxRun.yaml` file.

``` yaml
ESMX:
  App:
    logKindFlag:            ESMF_LOGKIND_Multi
    logAppendFlag:          false
    logFlush:               true
    startTime: 2017-10-01T00:00:00
    stopTime:  2017-10-02T00:00:00

  Driver:
    componentList: [HYD, GWR]
    attributes:
      Verbosity: low
      Profiling: 0
      cpl_list: CPL_LIST_NNE
    runSequence: |
      @3600
        HYD
        HYD -> GWR :remapmethod=bilinear:srcMaskValues=0:dstMaskValues=0
        GWR
      @

HYD:
  model: wrfhydro
  petList: [0-4]
  attributes:
    # Verbosity: "low"
    # Diagnostic: "max"
    # Profiling: "0"
    # realize_all_export: false
    config_file: "hydro.namelist"
    das_config_file: "namelist.hrldas"
    time_step: "0"
    forcings_directory: "none"
    domain_id: "1"
    # nest_to_nest: false
    # import_dependency: false
    # write_restart: false
    # read_restart: false
    # input_directory: "./HYD_CAP_INPUT"
    # output_directory: "./HYD_CAP_OUTPUT"

GWR:
  model: parflow
  petList: [5-8]
  attribute:
    Verbosity: 1
    Diagnostic: max
    realize_all_import: false
    realize_all_export: false
    filename: LW
    initialize_import: FLD_INIT_ZERO
```
