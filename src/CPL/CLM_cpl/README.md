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
