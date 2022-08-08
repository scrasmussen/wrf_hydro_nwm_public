#!/bin/bash
set -e
compare_py=/glade/u/home/gaydos/hytest/plotting/generate_diff_plots.py

compare_files_in() {
    test_case=$3
    dir1=$1/example_case/${test_case}
    dir2=$2/example_case/${test_case}
    type='ldas'
    echo "  Examining $(basename ${dir1})"
    # cmd="nccmp -dfsSq ${restarts1[i]} ${restarts2[i]}"
    cmd="python3 ${compare_py} \
    -o compare/${1#build-}${2#build-}/${3} \
    -b ${dir1} \
    -c ${dir2} \
    -f ${type}"
    # echo "$ ${cmd}"
    if ! eval ${cmd}; then
    	echo "ERROR: ${cmd}"
    	# exit 1
    fi
}

print_help_manual(){
    echo " OVERVIEW: compares CMake output created with 'make test-model' with"
    echo "           Python script generate_diff_plots.py."
    echo "           If no options are passed all directories matching build-*"
    echo "           are compared to each other"
    echo " USAGE: bash cmp-results.sh [options] ..."
    echo ""
    echo " OPTIONS:"
    echo "   -d, --dirs dir1 dir2 [dir3...dirn] : compare passed directories"
    echo "   -h, --help: print this help message and exit"
    echo "   -p, --pattern arg : compare matching arg* directories"
}


# variables
test_cases=(Gridded Gridded_no_lakes NWM Reach ReachLakes)

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
    '--help')     set -- "$@" '-h'   ;;
    '--dirs')     set -- "$@" '-d'   ;;
    '--pattern')  set -- "$@" '-p'   ;;
    *)            set -- "$@" "$arg" ;;
  esac
done

# parse arguments
OPTIND=1
while getopts "d:hp:" opt; do
    case "${opt}" in
	'h') print_help_manual; exit 0;;
	'd') dirs_option=1;;
	'p') regex_option=1;;
	'?') echo 'Error: unable to parse arguments'; exit 1;;
    esac
done

# Find the correct directories to examine
if [[ ${dirs_option} == 1 && ${regex_option} == 1 ]]; then
    echo "Error: list directories only or give regex only"
    exit 1
elif [[ ${dirs_option} == 1 ]]; then
    tmp_dirs=("${@#-d}")
    tmp_dirs2=("${tmp_dirs[@]%/}")
    build_dirs=(${tmp_dirs2[@]})
elif [[ ${regex_option} == 1 ]]; then
    tmp_regex="${@#-p}"
    regex="${tmp_regex## }"
    build_dirs=($(ls -d ${regex}*))
else
    build_dirs=($(ls -d build*))
fi

# compare every permutatio of directories
n_build_dirs=${#build_dirs[@]}
for (( i=0; i<${n_build_dirs}; i++ )); do
    for (( j=i+1; j<${n_build_dirs}; j++ )); do
	echo "Comparing ${build_dirs[i]} and ${build_dirs[j]}:"
	for test_case in ${test_cases[@]}; do
	    compare_files_in "${build_dirs[i]}" \
			     "${build_dirs[j]}" \
			     ${test_case}
	done
    done
done

echo "Fin"
