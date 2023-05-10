from . import bmi
import ctypes as ct
import sys
bind_c_lib_path_1='../../../../build/lib/libbmi_wrf_hydro_nwm_bind_c.so'
bind_c_lib_path_2='../lib/libbmi_wrf_hydro_nwm_bind_c.so'
try:
    wrf_h = ct.CDLL(bind_c_lib_path_1)
except:
    wrf_h = ct.CDLL(bind_c_lib_path_2)


# ---------------------------------
# --- Implemented BMI Functions ---
# ---------------------------------

# Get the name of the model.
wrf_h.get_component_name.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)]
wrf_h.get_component_name.restype = ct.c_int
def get_component_name():
    model_name = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)()
    wrf_h.get_component_name(ct.byref(model_name))
    return model_name.value

# Perform startup tasks for the model.
wrf_h.initialize.argtypes = [] # need to work on character strings
wrf_h.initialize.restype = ct.c_int
def initialize():
    wrf_h.initialize()

# Start time of the model.
wrf_h.get_start_time.argtypes = [ct.POINTER(ct.c_double)]
wrf_h.get_start_time.restype = ct.c_int
def get_start_time():
    current_time = ct.c_double()
    wrf_h.get_start_time(ct.byref(current_time))
    return current_time.value

# End time of the model.
wrf_h.get_end_time.argtypes = [ct.POINTER(ct.c_double)]
wrf_h.get_end_time.restype = ct.c_int
def get_end_time():
    end_time = ct.c_double()
    wrf_h.get_end_time(ct.byref(end_time))
    return end_time.value

# Count a model's input variables.
wrf_h.get_input_item_count.argtypes = [ct.POINTER(ct.c_int)]
wrf_h.get_input_item_count.restype = ct.c_int
def get_input_item_count():
    input_item_count = ct.c_int()
    wrf_h.get_input_item_count(input_item_count)
    return input_item_count.value

# Count a model's output variables.
wrf_h.get_output_item_count.argtypes = [ct.POINTER(ct.c_int)]
wrf_h.get_output_item_count.restype = ct.c_int
def get_output_item_count():
    output_item_count = ct.c_int()
    wrf_h.get_output_item_count(output_item_count)
    return output_item_count.value

# Advance the model one time step.
wrf_h.update.argtypes = []
wrf_h.update.restype = ct.c_int
def update():
    wrf_h.update()

# Current time of the model.
wrf_h.get_current_time.argtypes = [ct.POINTER(ct.c_double)]
wrf_h.get_current_time.restype = ct.c_int
def get_current_time():
    current_time = ct.c_double()
    wrf_h.get_current_time(ct.byref(current_time))
    return current_time.value

# -------------------------------
# --- Working On Implementing ---
# -------------------------------

# Perform teardown tasks for the model.
wrf_h.finalize.argtypes = []
wrf_h.finalize.restype = ct.c_int
def finalize():
    print("FINALIZE BROKEN FOR SOME REASON")
    res = wrf_h.finalize()
    print("JK, ITS WORKING")


# List a model's input variables.
def get_input_var_names():
    input_item_count = get_input_item_count()
    print("iic is", input_item_count)
    wrf_h.get_input_var_names.argtypes = \
        [(ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME *
                     input_item_count))]
    wrf_h.get_input_var_names.restype = ct.c_int

    names = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME *
                     input_item_count)()
    wrf_h.get_input_var_names(ct.byref(names))
    print("---Python Todo---")
    # names are returned from the function in a tuple, a standard container in
    # the language.

# Get the grid identifier for the given variable.
wrf_h.get_var_grid.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_var_grid.restype = ct.c_int
def get_var_grid(name):
    var_name = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)()
    var_name.value = name.encode('utf-8')
    grid_num = ct.c_int()
    wrf_h.get_var_grid(ct.byref(var_name), ct.byref(grid_num))
    return grid_num.value

# Get the total number of elements in the computational grid.
wrf_h.get_grid_size.argtypes = \
    [ct.POINTER(ct.c_int), ct.POINTER(ct.c_int)]
wrf_h.get_grid_size.restype = ct.c_int


# Get a copy of values (flattened!) of the given integer variable.
wrf_h.get_value_int.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_value_int.restype = ct.c_int
def get_value(var_name):
    grid_num = ct.c_int()
    grid_size = ct.c_int()
    grid_num.value = 1
    wrf_h.get_grid_size(grid_num, grid_size)
    print(" IN PYTHON GET VALUE OF", var_name.value, "num",
          grid_num.value, "size", grid_size.value)
wrf_h.get_value = get_value


# Template for BMI Python C function definitions
# wrf_h..argtypes = []
# wrf_h..restype = ct.c_int
