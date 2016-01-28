SHELL=/bin/bash

compile-all:
	for file in *f90; do gfortran $$file -o ../../$${file%.f90}; done

