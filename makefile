#COMPILER=gfortran
COMPILER=/usr/local/include/gcc_4.7/gcc-4.7/bin/gfortran -std=f2003

clean: read_file
	rm *.o *.mod

# Main program + deps
read_file: hecras_IO_b.o global_defs.o IO_util.o river_classes.o read_file.f95
	$(COMPILER) $^ -o $@

hecras_IO_b.o: global_defs.o IO_util.o river_classes.o hecras_IO_b.f95
	$(COMPILER) -c hecras_IO_b.f95 

IO_util.o: global_defs.o IO_util.f95
	$(COMPILER) -c IO_util.f95

river_classes.o: global_defs.o river_classes.f95
	$(COMPILER) -c river_classes.f95 

global_defs.o: global_defs.f95
	$(COMPILER) -c $^


