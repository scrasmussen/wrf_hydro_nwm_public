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

# Perform startup tasks for the model.
wrf_h.initialize.argtypes = [] # need to work on character strings
wrf_h.initialize.restype = ct.c_int
def initialize():
    wrf_h.initialize()

# Advance the model one time step.
wrf_h.update.argtypes = []
wrf_h.update.restype = ct.c_int
def update():
    wrf_h.update()

# Advance the model until the given time.
wrf_h.update_until.argtypes = [ct.POINTER(ct.c_double)]
wrf_h.update_until.restype = ct.c_int
def update_until(time):
    time = ct.c_double(time)
    wrf_h.update_until(time)

# todo: finalize, need to debug

# Get the name of the model.
wrf_h.get_component_name.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)]
wrf_h.get_component_name.restype = ct.c_int
def get_component_name():
    model_name = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)()
    wrf_h.get_component_name(ct.byref(model_name))
    return model_name.value

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

# todo: get_input_var_names
# todo: get_output_var_names

# Get the grid identifier for the given variable.
wrf_h.get_var_grid.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_var_grid.restype = ct.c_int
def get_var_grid(name):
    var_name = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME)()
    var_name.value = name.encode()
    grid = ct.c_int()
    wrf_h.get_var_grid(ct.byref(var_name), ct.byref(grid))
    return grid.value

# Get the data type of the given variable as a string.
wrf_h.get_var_type.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_VAR_NAME),
     ct.POINTER(ct.c_char * bmi.BMI_MAX_TYPE_NAME)]
wrf_h.get_var_type.restype = ct.c_int
def get_var_type(name):
    var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    var_name.value = name.encode()
    var_type = ct.create_string_buffer(bmi.BMI_MAX_TYPE_NAME)
    wrf_h.get_var_type(ct.byref(var_name), ct.byref(var_type))
    return var_type.value.decode()

# Get the units of the given variable.
wrf_h.get_var_units.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_VAR_NAME),
     ct.POINTER(ct.c_char * bmi.BMI_MAX_UNITS_NAME)]
wrf_h.get_var_units.restype = ct.c_int
def get_var_units(name):
    var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    var_name.value = name.encode()
    units = ct.create_string_buffer(bmi.BMI_MAX_UNITS_NAME)
    wrf_h.get_var_units(ct.byref(var_name), ct.byref(units))
    return units.value.decode()

# Get memory use per array element, in bytes.
wrf_h.get_var_itemsize.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_VAR_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_var_itemsize.restype = ct.c_int
def get_var_itemsize(name):
    var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    var_name.value = name.encode()
    var_size = ct.c_int()
    wrf_h.get_var_itemsize(ct.byref(var_name), ct.byref(var_size))
    return var_size.value

# Get size of the given variable, in bytes.
wrf_h.get_var_nbytes.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_VAR_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_var_nbytes.restype = ct.c_int
def get_var_nbytes(name):
    var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    var_name.value = name.encode()
    nbytes = ct.c_int()
    wrf_h.get_var_nbytes(ct.byref(var_name), ct.byref(nbytes))
    return nbytes.value

# todo: needs to implemented in Fortran
# Describe where a variable is located: node, edge, or face.
# wrf_h.get_var_location.argtypes = [ct.POINTER(ct.)]
# wrf_h.get_var_location.restype = ct.c_int
# def get_var_location(name):
#     var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
#     var_name.value = name.encode()
#     location = ct.create_string_buffer(bmi.BMI_MAX_TYPE_NAME)
#     wrf_h.get_var_location(ct.byref(name), ct.byref(location))
#     return location.value.decode()

# Current time of the model.
wrf_h.get_current_time.argtypes = [ct.POINTER(ct.c_double)]
wrf_h.get_current_time.restype = ct.c_int
def get_current_time():
    current_time = ct.c_double()
    wrf_h.get_current_time(ct.byref(current_time))
    return current_time.value

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

# Time units of the model.
wrf_h.get_time_units.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_UNITS_NAME)]
wrf_h.get_time_units.restype = ct.c_int
def get_time_units():
    var_units = ct.create_string_buffer(bmi.BMI_MAX_UNITS_NAME)
    wrf_h.get_time_units(ct.byref(var_units))
    return var_units.value.decode()

# Time units of the model.
wrf_h.get_time_step.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_UNITS_NAME)]
wrf_h.get_time_step.restype = ct.c_int
def get_time_step():
    var_units = ct.create_string_buffer(bmi.BMI_MAX_UNITS_NAME)
    wrf_h.get_time_step(ct.byref(var_units))
    return var_units.value.decode()

def get_value(name, array):
    var_type = get_var_type(name)
    print("---", var_type)
    if (var_type == 'integer'):
        array = get_value_int(name, array)
    elif (var_type == 'float'):
        array = get_value_float(name, array)
    elif (var_type == 'double'):
        array = get_value_double(name, array)
    return array

# todo: get_value_int
# Get a copy of values (flattened!) of the given integer variable.
wrf_h.get_value_int.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_value_int.restype = ct.c_int
def get_value_int(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_int(ct.byref(var_name), ct.byref(array))
    return array

# todo: get_value_float
# Get a copy of values (flattened!) of the given float variable.
wrf_h.get_value_float.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_float)]
wrf_h.get_value_float.restype = ct.c_int
def get_value_float(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_float(ct.byref(var_name), ct.byref(array))
    return array

# todo: get_value_double
# Get a copy of values (flattened!) of the given double variable.
wrf_h.get_value_double.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_double)]
wrf_h.get_value_double.restype = ct.c_int
def get_value_double(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_double(ct.byref(var_name), ct.byref(array))
    return array

# todo: get_value_ptr_int
# todo: get_value_ptr_float
# todo: get_value_ptr_double

def get_value_at_indices(name, array):
    var_type = get_var_type(name)
    print("---", var_type)
    if (var_type == 'integer'):
        array = get_value_at_indices_int(name, array)
    elif (var_type == 'float'):
        array = get_value_at_indices_float(name, array)
    elif (var_type == 'double'):
        array = get_value_at_indices_double(name, array)
    return array

# todo: get_value_at_indices_int
# Get a copy of values (flattened!) of the given integer variable.
wrf_h.get_value_at_indices_int.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.get_value_at_indices_int.restype = ct.c_int
def get_value_at_indices_int(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_at_indices_int(ct.byref(var_name), ct.byref(array))
    return array

# todo: get_value_at_indices_float
# Get a copy of values (flattened!) of the given float variable.
wrf_h.get_value_at_indices_float.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_float)]
wrf_h.get_value_at_indices_float.restype = ct.c_int
def get_value_at_indices_float(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_at_indices_float(ct.byref(var_name), ct.byref(array))
    return array

# todo: get_value_at_indices_double
# Get a copy of values (flattened!) of the given double variable.
wrf_h.get_value_at_indices_double.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_double)]
wrf_h.get_value_at_indices_double.restype = ct.c_int
def get_value_at_indices_double(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.get_value_at_indices_double(ct.byref(var_name), ct.byref(array))
    return array

def set_value(name, array):
    var_type = get_var_type(name)
    print("---", var_type)
    if (var_type == 'integer'):
        array = set_value_int(name, array)
    elif (var_type == 'float'):
        array = set_value_float(name, array)
    elif (var_type == 'double'):
        array = set_value_double(name, array)
    return array

# todo: set_value_int
# Get a copy of values (flattened!) of the given integer variable.
wrf_h.set_value_int.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.set_value_int.restype = ct.c_int
def set_value_int(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_int(ct.byref(var_name), ct.byref(array))
    return array

# todo: set_value_float
# Get a copy of values (flattened!) of the given float variable.
wrf_h.set_value_float.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_float)]
wrf_h.set_value_float.restype = ct.c_int
def set_value_float(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_float(ct.byref(var_name), ct.byref(array))
    return array

# todo: set_value_double
# Get a copy of values (flattened!) of the given double variable.
wrf_h.set_value_double.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_double)]
wrf_h.set_value_double.restype = ct.c_int
def set_value_double(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_double(ct.byref(var_name), ct.byref(array))
    return array

# todo: set_value_ptr_int
# todo: set_value_ptr_float
# todo: set_value_ptr_double

def set_value_at_indices(name, array):
    var_type = get_var_type(name)
    print("---", var_type)
    if (var_type == 'integer'):
        array = set_value_at_indices_int(name, array)
    elif (var_type == 'float'):
        array = set_value_at_indices_float(name, array)
    elif (var_type == 'double'):
        array = set_value_at_indices_double(name, array)
    return array

# todo: set_value_at_indices_int
# Get a copy of values (flattened!) of the given integer variable.
wrf_h.set_value_at_indices_int.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_int)]
wrf_h.set_value_at_indices_int.restype = ct.c_int
def set_value_at_indices_int(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_at_indices_int(ct.byref(var_name), ct.byref(array))
    return array

# todo: set_value_at_indices_float
# Get a copy of values (flattened!) of the given float variable.
wrf_h.set_value_at_indices_float.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_float)]
wrf_h.set_value_at_indices_float.restype = ct.c_int
def set_value_at_indices_float(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_at_indices_float(ct.byref(var_name), ct.byref(array))
    return array

# todo: set_value_at_indices_double
# Get a copy of values (flattened!) of the given double variable.
wrf_h.set_value_at_indices_double.argtypes = \
    [ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME),
     ct.POINTER(ct.c_double)]
wrf_h.set_value_at_indices_double.restype = ct.c_int
def set_value_at_indices_double(name, array):
    # var_name = ct.create_string_buffer(bmi.BMI_MAX_VAR_NAME)
    # var_name.value = name.encode()
    # get_grid_size(name, grid_size)
    # array = ct.
    # wrf_h.set_value_at_indices_double(ct.byref(var_name), ct.byref(array))
    return array

# Get number of dimensions of the computational grid.
wrf_h.get_grid_rank.argtypes = \
    [ct.POINTER(ct.c_int), ct.POINTER(ct.c_int)]
wrf_h.get_grid_rank.restype = ct.c_int
def get_grid_rank(grid):
    var_grid = ct.c_int(grid)
    rank = ct.c_int()
    wrf_h.get_grid_rank(ct.byref(var_grid), ct.byref(rank))
    return rank.value

# Get the total number of elements in the computational grid.
wrf_h.get_grid_size.argtypes = \
    [ct.POINTER(ct.c_int), ct.POINTER(ct.c_int)]
wrf_h.get_grid_size.restype = ct.c_int
def get_grid_size(grid):
    grid = ct.c_int(grid)
    size = ct.c_int()
    wrf_h.get_grid_size(ct.byref(grid), ct.byref(size))
    return size.value

# Get the grid type as a string.
wrf_h.get_grid_type.argtypes = \
    [ct.POINTER(ct.c_int),
     ct.POINTER(ct.c_char * bmi.BMI_MAX_TYPE_NAME)]
wrf_h.get_grid_type.restype = ct.c_int
def get_grid_type(grid):
    var_grid = ct.c_int(grid)
    grid_type = ct.create_string_buffer(bmi.BMI_MAX_TYPE_NAME)
    wrf_h.get_grid_type(ct.byref(var_grid), ct.byref(grid_type))
    return grid_type.value.decode()

# todo: get_grid_type
# todo: get_grid_spacing
# todo: get_grid_origin
# todo: get_grid_x
# todo: get_grid_y
# todo: get_grid_z
# todo: get_grid_node_count
# todo: get_grid_edge_count
# todo: get_grid_face_count
# todo: get_grid_edge_nodes
# todo: get_grid_face_nodes
# todo: get_grid_nodes_per_face


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

# list a model's input variables.
wrf_h.get_input_var_names.restype = ct.c_int
def get_input_var_names():
    # input items is input_item_count long, have to define func argtype after
    # obtaining that number
    input_item_count = get_input_item_count()
    wrf_h.get_input_var_names.argtypes = \
        [(ct.POINTER(ct.c_char * bmi.BMI_MAX_COMPONENT_NAME *
                     input_item_count))]
    names = (ct.c_char * bmi.BMI_MAX_COMPONENT_NAME *
                     input_item_count)()
    wrf_h.get_input_var_names(ct.byref(names))
    print("---Python Todo---")
    # print(names[1:3])
    # print(''.join(names).split(b"\0"))
    # print(''.join(str(names)).split(b"\0"))
    # regular_array = bytearray.from_buffer(c_char_array)
    # print(names)
    # names are returned from the function in a tuple, a standard container in
    # the language.
    a = ['foobar' for i in range(input_item_count)]
    return a

# ------


# Template for BMI Python C function definitions
# wrf_h..argtypes = [ct.POINTER(ct.)]
# wrf_h..restype = ct.c_int
