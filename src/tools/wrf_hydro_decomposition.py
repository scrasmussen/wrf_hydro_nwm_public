#!/usr/bin/python3
import wrf_hydro_tools
import ctypes as ct
import numpy as np

import argparse
import sys


def main():


    print('--- Starting Python Decomposition Tool---')
    np_int = parseCLA()
    np_int = int(np_int)

    print("Reporting decomposition of", np_int, "processes")
    np_c = ct.c_int(np_int)
    x_np = ct.c_int(np_int)
    y_np = ct.c_int(np_int)
    nx = ct.c_int()
    ny = ct.c_int()
    start_x = np.zeros(np_int, dtype=ct.c_int)
    start_y = np.zeros(np_int, dtype=ct.c_int)
    end_x = np.zeros(np_int, dtype=ct.c_int)
    end_y = np.zeros(np_int, dtype=ct.c_int)

    # wrf_hydro_tools.

    print("np_c =", np_c.value)
    # print("x_np =", x_np, "y_np =", y_np)
    # print("nx =", nx.value, "ny =", ny.value)
    # print("start_x =", start_x, "start_y =", start_y)
    # print("end_x =", end_x, "end_y =", end_y)

    # test_np, x_np = wrf_hydro_tools.get_domain_decomposition(np_c)
    wrf_hydro_tools.get_domain_decomposition(np_c,
                                             x_np, y_np,
                                             nx, ny,
                                             start_x, start_y,
                                             end_x, end_y)

    print('--- Finished Running Python Tool ---')
    print('x_np =', x_np)
    print('y_np =', y_np)
    print('nx =', nx)
    print('ny =', ny)
    print("start_x", start_x)
    print("start_y", start_y)
    print("end_x", end_x)
    print("end_y", end_y)


    # parse command line arguments
def parseCLA():
    parser = argparse.ArgumentParser(description="Report domain decomposition from WRF-Hydro.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-np", help="Number of processes")
    # group.add_argument("-p", help="Number of processes")

    # Parse the arguments
    args = parser.parse_args()

    return args.np


if __name__ == "__main__":
    main()
