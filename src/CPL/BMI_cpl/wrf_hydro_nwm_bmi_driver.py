#!/usr/bin/python3
import bmi_wrf_hydro_nwm
import ctypes as ct


def main():
    # ----------------------------------------------------------------------
    #  define variables for later use
    current_time = ct.c_double()
    end_time = ct.c_double()
    # ----------------------------------------------------------------------

    print('--- Start in Python---')
    wrf_hydro = bmi_wrf_hydro_nwm.symbols_init()

    wrf_hydro.initialize()
    wrf_hydro.get_start_time(ct.byref(current_time))
    wrf_hydro.get_end_time(ct.byref(end_time))
    print("start time is", current_time.value, "end time is", end_time.value)

    while (current_time.value < end_time.value):
        wrf_hydro.update()
        wrf_hydro.get_current_time(ct.byref(current_time))
        if current_time.value % 10 == 0:
            print("time =", current_time.value)

    wrf_hydro.finalize()
    print('--- Finished running Python test driver ---')

if __name__ == "__main__":
    main()
