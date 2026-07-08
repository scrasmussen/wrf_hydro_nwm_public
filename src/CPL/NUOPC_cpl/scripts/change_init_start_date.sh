#!/bin/bash

@echo "Make sure the module nco is loaded"
@echo ""

new_time='2013-09-10_00:00:00'
nc_file='frontrange.init.nc'

set -e -x

ncatted -O -a config_start_time,Time,o,c,${new_time} ${nc_file}
ncap2 -O -s 'initial_time="'${new_time}'"' ${nc_file} ${nc_file} 
ncap2 -O -s 'xtime="'${new_time}'"' ${nc_file} ${nc_file} 
