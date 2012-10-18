
read_file: hecras_IO_b.o global_defs.o read_file.f95
	gfortran $^ -o $@

hecras_IO_b.o: global_defs.o river_classes.o hecras_IO_b.f95
	gfortran -c hecras_IO_b.f95 

river_classes.o: global_defs.o river_classes.f95
	gfortran -c river_classes.f95 

global_defs.o: global_defs.f95
	gfortran -c $^
