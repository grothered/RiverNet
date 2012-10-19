clean: read_file
	rm *.o *.mod

# Main program + deps
read_file: hecras_IO_b.o global_defs.o IO_util.o read_file.f95
	gfortran $^ -o $@

hecras_IO_b.o: global_defs.o IO_util.o river_classes.o hecras_IO_b.f95
	gfortran -c hecras_IO_b.f95 

IO_util.o: global_defs.o IO_util.f95
	gfortran -c IO_util.f95

river_classes.o: global_defs.o river_classes.f95
	gfortran -c river_classes.f95 

global_defs.o: global_defs.f95
	gfortran -c $^


