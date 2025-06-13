#!/usr/bin/python3
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

# ---------------------------------
# --- Calling Fortran Functions ---
# ---------------------------------
def get_domain_decomposition(np_int):
    np_c_int = ct.c_int(np_int)

    wrf_h_tools.get_domain_decomposition.argtypes = [
        ct.POINTER(ct.c_int), # np_c_int
        ct.POINTER(ct.c_int), # x_np
        ct.POINTER(ct.c_int), # y_np
        ct.POINTER(ct.c_int), # nx
        ct.POINTER(ct.c_int), # ny
        ct.POINTER(ct.c_int*np_int), # start_x
        ct.POINTER(ct.c_int*np_int), # start_y
        ct.POINTER(ct.c_int*np_int), # end_x
        ct.POINTER(ct.c_int*np_int), # end_y
    ]

    x_np = ct.c_int()
    y_np = ct.c_int()
    nx = ct.c_int()
    ny = ct.c_int()
    start_x = np.zeros(np_int, dtype=ct.c_int)
    start_y = np.zeros(np_int, dtype=ct.c_int)
    end_x = np.zeros(np_int, dtype=ct.c_int)
    end_y = np.zeros(np_int, dtype=ct.c_int)

    wrf_h_tools.get_domain_decomposition(
        ct.byref(np_c_int),
        ct.byref(x_np),
        ct.byref(y_np),
        ct.byref(nx),
        ct.byref(ny),
        start_x.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        start_y.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        end_x.ctypes.data_as(ct.POINTER(ct.c_int*np_int)),
        end_y.ctypes.data_as(ct.POINTER(ct.c_int*np_int)))

    return x_np.value, y_np.value, nx.value, ny.value, \
        start_x, start_y, end_x, end_y

# Template for Python C function definitions
# wrf_h_tools..argtypes = [ct.POINTER(ct.)]
# wrf_h_tools..restype = ct.c_int
