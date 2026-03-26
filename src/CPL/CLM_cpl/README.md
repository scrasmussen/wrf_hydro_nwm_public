# Build CTSM
This build follows the [ctsm build instructions](https://escomp.github.io/CTSM/lilac/obtaining-building-and-running/obtaining-and-building-ctsm.html#obtaining-and-building-ctsm-and-lilac).

```bash
$ git clone https://github.com/ESCOMP/CTSM.git ctsm
$ cd ctsm
$ ./bin/git-fleximod update
$ ./lilac/build_ctsm --machine derecho --compiler gnu /glade/scratch/${USER}/ctsm_build_dir
...
clm built in 65.982331 seconds

$ source /glade/derecho/scratch/${USER}/ctsm_build_dir/ctsm_build_environment.sh
```

# Coupling Build
This currently will not build since the clm3 variable does not exist, the previous
coupling effort uses clm3 (2004-2010).
The CLM modules in `module_clm_HYDRO.F` exist except for `clmtype` which uses
`clm3`.

| module        | element           | description                                                        |
|---------------|-------------------|--------------------------------------------------------------------|
| domainMod     | ldomain           | main CLM land domain descriptor, holding domain/layout information |
| clmtype       | clm3              |                                                                    |
| clm_varpar    | nlevgrnd          | number of ground layers                                            |
| decompMod     | get\_proc\_bounds | current processor beg and end gridcell, landunit, column, patch    |
| decompMod     | get\_proc\_global | total gridcells, landunits, columns, patchs                        |
| clm\_varcon   | zsoi              | depths of CLM soil levels                                          |
| subgridAveMod | l2g               | perform an average landunits to gridcells                          |
| subgridAveMod | c2g               | perform an average columns to gridcells                            |


## Procedure
The coupled driver
 - maps CLMs runoff and soil state fields onto the hydro grid
 - runs WRF-Hydro for one coupling step
 - maps updated soil states back into CLM

### CLM Field -> Hydro Grid

| CLM Field   | Hydro Grid                   |
|-------------|------------------------------|
| qflx\_surf  | rt\_domain(did)%infxsrt      |
| qflx\_drain | rt\_domain(did)%soldrain     |
| t\_soisno   | rt\_domain(did)%stc          |
| h2osoi\_vol | rt\_domain(did)%smc          |
| h2osoi\_liq | rt\_domain(did)%sh2ox / 1000 |

### Hydro Grid -> CLM Field
Set `clm_lev = 10`.

| Hydro Grid                   | CLM Field   |
|------------------------------|-------------|
| rt\_domain(did)%stc          | t\_soisno   |
| rt\_domain(did)%smc          | h2osoi\_vol |
| rt\_domain(did)%sh2ox * 1000 | h2osoi\_liq |


## WRF-Hydor Imports/Exports
### WRF-Hydro Imports
Variables are members of `rt\_domain(did)`
| Hydro Grid   | Description                 |
|--------------|-----------------------------|
| infxsrt      | infiltration excess water   |
| soldrain     | soil drainage               |
| stc          | soil temperature            |
| smc          | total liq+ice soil moisture |
| sh2ox / 1000 | liquid soil moisture        |

### WRF-Hydro Exports
| Hydro Grid                   |
|------------------------------|
| rt\_domain(did)%stc          |
| rt\_domain(did)%smc          |
| rt\_domain(did)%sh2ox * 1000 |


# CTSM and WRF-Hydro ESMF Coupling
This coupling will use ESMF and NUOPC
## CTSM Exports
The following table matches the CLM field WRF-Hydro needs with the current
fields being exported by CTSM ESMF.

| CLM Field   | Export Fields?                     | Full or Partial Analog |
|-------------|------------------------------------|------------------------|
| qflx\_surf  | Flrl\_rofsur - qflx\_over          | Partial                |
| qflx\_drain | Flrl\_rofsub - qflx\_perched_drain | Partial                |
| t\_soisno   | Sl\_soilw is only the first layer  | None                   |
| h2osoi\_vol | Sl\_soilw                          | Partial                |
| h2osoi\_liq | None                               | None                   |

| CTSM Field   | Variable Definition                                         | Export Issue                             |
|--------------|-------------------------------------------------------------|------------------------------------------|
| FLrl\_rofsur | waterlnd2atmbulk\_inst%qflx\_rofliq\_qsur\_grc(begg:)       | sum of qflx\_over and qflx\_h2osfc\_surf |
| Flrl\_rofsub | waterlnd2atmbulk\_inst%qflx\_rofliq\_qsub\_grc(g) + &       | sum of qsub\_grc and drain\_perched\_grc |
|              | waterlnd2atmbulk\_inst%qflx\_rofliq\_drain\_perched\_grc(g) |                                          |
| Sl\_soilw    | waterlnd2atmbulk\_inst%h2osoi\_vol\_grc(begin:, 1)          | only first layer                         |
|              |                                                             |                                          |
|              |                                                             |                                          |
|              |                                                             |                                          |

| Hydro Var | CTSM Var                            | CTSM Type         | description                                                                      |
|-----------|-------------------------------------|-------------------|----------------------------------------------------------------------------------|
| infxsrt   | qflx\_infl\_col                     | waterflux\_type   | col infiltration (mm H2O /s)                                                     |
| soldrain  | qflx\_drain\_col                    | waterflux\_type   | col sub-surface runoff (mm H2O /s)                                               |
| stc       | t\_soisno\_col                      | temperature\_type | col soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)                             |
| smc       | h2osoi\_liq\_col + h2osoi\_ice\_col |                   |                                                                                  |
|           | h2osoi\_liq\_col                    | waterstate\_type  | col liquid water (kg/m2) (new) (-nlevsno+1:nlevgrnd)                             |
|           | h2osoi\_ice\_col                    | waterstate\_type  | col ice lens (kg/m2) (new) (-nlevsno+1:nlevgrnd)                                 |
| sh2ox     | h2osoi\_vol\_col ?                  | waterstate\_type  | col volumetric soil water (0<=h2osoi\_vol<=watsat) [m3/m3]  (nlevgrnd)           |
| sh2ox     | h2osoi\_vol\_prs\_grc ?             | waterstate\_type  | grc volumetric soil water prescribed (0<=h2osoi_vol<=watsat) [m3/m3]  (nlevgrnd) |
