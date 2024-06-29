# from . import bmi
import ctypes as ct
import numpy as np
import sys
if sys.version_info.major == 2:
    raise RuntimeError("wrf-hydro-tools requires Python 3 or higher.")

bind_c_lib_path_1='../../../../build/lib/libwrf_hydro_tools_bind_c.so'
bind_c_lib_path_2='../lib/libwrf_hydro_tools_bind_c.so'
try:
    wrf_h_tools = ct.CDLL(bind_c_lib_path_1)
except:
    wrf_h_tools = ct.CDLL(bind_c_lib_path_2)

# def show_details(arr: np.ndarray):
#     first_col_width = 20
#     details = f"{'array':<{first_col_width}s}: {np.array_str(arr).replace(chr(10),',')}\n"\
#              +f"{'flags':<{first_col_width}s}: {arr.flags.f_contiguous = }, {arr.flags.c_contiguous = }\n"\
#              +f"{'array interface':<{first_col_width}s}: {arr.__array_interface__}\n" \
#              +f"{'strides':<{first_col_width}s}: {arr.strides}\n" \
#              +f"{'datatype':<{first_col_width}s}: {arr.dtype}\n" \
#              +f"{'base':<{first_col_width}s}: {np.array_str(arr.base).replace(chr(10),',') if arr.base is not None else '-'}\n"\
#              +f"{'base flags':<{first_col_width}s}: " + (f"{arr.base.flags.f_contiguous = }, {arr.base.flags.c_contiguous = }\n" if arr.base is not None else "-")
#     return details

# ---------------------------------
# --- Implemented BMI Functions ---
# ---------------------------------

test_np = 4
# Count a model's input variables.
# wrf_h_tools.get_domain_decomposition.argtypes = [
#     ct.POINTER(ct.c_int)]
    # ct.POINTER(ct.c_int * 2),
    # ct.POINTER(ct.c_int * 2),
    # ct.POINTER(ct.c_int),
    # ct.POINTER(ct.c_int),
    # ct.POINTER(ct.c_int *test_np),
    # ct.POINTER(ct.c_int *test_np),
    # ct.POINTER(ct.c_int *test_np),
    # ct.POINTER(ct.c_int *test_np)]
# wrf_h_tools.get_domain_decomposition.restype = [
#     ct.c_int, ct.c_int]
# wrf_h_tools.get_domain_decomposition.argtypes = [
#         ct.POINTER(ct.c_int),
#         ct.POINTER(ct.c_int * 2),
#         ct.POINTER(ct.c_int * 2)]

def get_domain_decomposition(np_in, x_np, y_np, nx, ny,
                             start_x, start_y, end_x, end_y):
    np_int = np_in.value
    np_val = ct.c_int(np_in.value)

    wrf_h_tools.get_domain_decomposition.argtypes = [
        ct.POINTER(ct.c_int),
        ct.POINTER(ct.c_int),
        ct.POINTER(ct.c_int),
        ct.POINTER(ct.c_int),
        ct.POINTER(ct.c_int),
        ct.POINTER(ct.c_int*np_int),
        ct.POINTER(ct.c_int*np_int),
        ct.POINTER(ct.c_int*np_int),
        ct.POINTER(ct.c_int*np_int),
    ]

    # wrf_h_tools.get_domain_decomposition.restype = [
    #     ct.c_int, ct.c_int]

    # x_np = np.zeros(2, dtype=ct.c_int)
    # y_np = np.zeros(2, dtype=ct.c_int)
    # nx = ct.c_int()
    # ny = ct.c_int()
    # start_x = np.zeros(np_int, dtype=ct.c_int)
    # start_y = np.zeros(np_int, dtype=ct.c_int)
    # end_x = np.zeros(np_int, dtype=ct.c_int)
    # end_y = np.zeros(np_int, dtype=ct.c_int)

    # wrf_h.get_grid_rank(ct.byref(var_grid), ct.byref(rank))
    wrf_h_tools.get_domain_decomposition(
        ct.byref(np_val),
        ct.byref(x_np),
        ct.byref(y_np),
        ct.byref(nx),
        ct.byref(ny),
        start_x.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        start_y.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        end_x.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        end_y.ctypes.data_as(ct.POINTER(ct.c_int*np_int)))
    print("py x_np", x_np)
    print("py y_np", y_np)
    print("py start_x", start_x)
    print("py end_x", end_x)
    return
# ------

# wrf_h.get_grid_shape.argtypes = \
#     [ct.POINTER(ct.c_int),
#      ct.POINTER(ct.c_int * 3)]
# wrf_h.get_grid_shape.restype = ct.c_int
# def get_grid_shape(grid):  # foobar
#     grid = ct.c_int(grid)
#     shape = np.zeros(3, dtype=ct.c_int)
#     wrf_h.get_grid_shape(ct.byref(grid), shape.ctypes.data_as(ct.POINTER(ct.c_int*3)))
#     if shape[2] == 0:
#         return shape[:-1]
#     return shape



# Template for BMI Python C function definitions
# wrf_h_tools..argtypes = [ct.POINTER(ct.)]
# wrf_h_tools..restype = ct.c_int
