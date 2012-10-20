COMPILER=gfortran
#COMPILER=/usr/local/include/gcc_4.7/gcc-4.7/bin/gfortran -std=f2003

clean: read_file
	rm *.o *.mod

# Main program + deps
read_file: hecras_IO_b.o global_defs.o IO_util.o river_classes.o read_file.f03
	$(COMPILER) $^ -o $@

hecras_IO_b.o: global_defs.o IO_util.o river_classes.o hecras_IO_b.f03
	$(COMPILER) -c hecras_IO_b.f03 

IO_util.o: global_defs.o IO_util.f03
	$(COMPILER) -c IO_util.f03

river_classes.o: global_defs.o river_classes.f03
	$(COMPILER) -c river_classes.f03 

global_defs.o: global_defs.f03
	$(COMPILER) -c $^


