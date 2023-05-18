# MODFLOW Coupling


## Dependecies
In addition to the WRF-Hydro dependencies the following is needed to build and couple with MODFLOW.

| Libraries/Software | Version |
|--------------------|---------|
| Conda              |         |
| CMake              | 3.12+   |

### Dependecy Build Instructions
Make sure to first load the set of modules that are used to build WRF-Hydro.

```
$ cd [user_conda_env_path]/envs
$ conda create --prefix ./envs
$ conda activate [user_conda_env_path]/envs
$ conda install meson
```

## Build
Obtain MODFLOW release or develop branch if desired. Note: release binaries will not work with coupling because Fortran modules are needed, not just the shared libraries.

```
Build MODFLOW
$ cd [MODFLOW base directory]
$ meson setup builddir
$ meson compile -C builddir
$ export MODFLOW_BMI=[MODFLOW base directory]/builddir/srcbmi/

Build WRF-Hydro with MODFLOW Coupling
$ cd [WRF-Hydro base directory]
$ mkdir build
$ cd build
$ cmake .. -DMODFLOW=1
$ make -j 4
```
