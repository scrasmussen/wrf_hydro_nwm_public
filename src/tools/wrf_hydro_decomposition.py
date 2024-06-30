#!/usr/bin/python3
import wrf_hydro_tools
import ctypes as ct
import numpy as np

import argparse
import sys


def main():
    print('--- Starting Python Decomposition Tool---')
    np_s = parseCLA()
    np_int = int(np_s)

    print("Reporting decomposition of", np_int, "processes")

    x_np, y_np, nx, ny, start_x, start_y, end_x, end_y  = \
        wrf_hydro_tools.get_domain_decomposition(np_int)

    print('--- Finished calling get_domain_decomposition, result are:')
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
