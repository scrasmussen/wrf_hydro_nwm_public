#!/bin/bash


compilers=("Intel" "GNU")
compilers_l=$(echo ${compilers[@]} | tr '[:upper:]' '[:lower:]')

echo "Creating directory structure"

for c in ${compilers_l[@]}; do
    (set -x;
     rm -f "build_compilers/$c";
     mkdir -p "build_compilers/$c")
done


for c in ${compilers_l[@]}; do
    (cd build_compilers/$c
     if [[ "$c" == "gnu" ]]; then
	 echo "Building with GNU"
	 module purge
	 module restore system
	 ml gnu openmpi
	 ml
     elif [[ "$c" == "intel" ]]; then
     	 echo "Building with Intel"
     	 module purge
     	 module restore system
     	 ml
     fi

     cmake ../../../
     make
    )
done



echo "fin"
