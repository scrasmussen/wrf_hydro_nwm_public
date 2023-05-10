# BMI Coupling README
The [Basic Model Interface](https://bmi.readthedocs.io/en/stable/) ([BMI](https://github.com/csdms/bmi)) is a standardized set of control and query functions.
This allows for different models to use the BMI functions to couple and interact together.


## Drivers
WRF-Hydro's driver is implemented using BMI's model control functions in the following languages:
  - Fortran
  - Python
  - C++


## Adding new variables
The following is the list of currently supported variables.
To add new ones please follow the steps listed in this section.

### List variables exposed to the BMI
| Variable |
|----------|
| IVGTYP   |
| ISLTYP   |
| GLACT    |

### Exposing Additional Variables
Here are the steps for exposing additional WRF-Hydro variables to BMI.
Each new variable will need a grid rank,

1. The variable will need an identifying grid number. Add a new `case` block in the function `wrf_hydro_var_grid` and choose the next number.
2. Using that grid number or the variable name, add the variable case in the following functions:
   - `get_grid_rank`
   - `get_grid_shape`
   - `get_var_size`
   - `wrf_hydro_var_units`
   - `wrf_hydro_grid_type`
   - `wrf_hydro_set_{int,float,double}`
   - `wrf_hydro_get_{int,float,double}`
   - `wrf_hydro_set_at_indices_{int,float,double}`
   - `wrf_hydro_get_at_indices_{int,float,double}`
3. Add variable name to this README's "List variables exposed to the BMI" table
