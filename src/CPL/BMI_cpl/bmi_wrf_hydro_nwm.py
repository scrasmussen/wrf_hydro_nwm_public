import ctypes as ct
bind_c_lib_path_1='../../../../build/lib/libbmi_wrf_hydro_nwm_bind_c.so'
bind_c_lib_path_2='../lib/libbmi_wrf_hydro_nwm_bind_c.so'
def symbols_init():
    # this needs to be fixed so script can run from anywhere
    try:
        wrf_h = ct.CDLL(bind_c_lib_path_1)
    except:
        wrf_h = ct.CDLL(bind_c_lib_path_2)
    # wrf_h.initialize.argtypes = [] # need to work on character strings
    wrf_h.initialize.restype = ct.c_int

    wrf_h.get_start_time.argtypes = [ct.POINTER(ct.c_double)]
    wrf_h.get_start_time.restype = ct.c_int

    wrf_h.get_end_time.argtypes = [ct.POINTER(ct.c_double)]
    wrf_h.get_end_time.restype = ct.c_int

    wrf_h.update.argtypes = []
    wrf_h.update.restype = ct.c_int

    wrf_h.get_current_time.argtypes = [ct.POINTER(ct.c_double)]
    wrf_h.get_current_time.restype = ct.c_int

    wrf_h.finalize.argtypes = []
    wrf_h.finalize.restype = ct.c_int

    # wrf_h..argtypes = []
    # wrf_h..restype = ct.c_int
    return wrf_h
