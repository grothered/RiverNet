COMPILER=gfortran
#COMPILER=/usr/local/include/gcc_4.7/gcc-4.7/bin/gfortran -std=f2003
SLATEC=libslatec.a

clean: main
	rm *.o *.mod

main: global_defs.o one_d_relation_class.o xsect_classes.o reach_boundary_classes.o river_classes.o IO_util.o hecras_IO_b.o main.f03
	$(COMPILER) $^  -o $@ $(SLATEC)
 
hecras_IO_b.o: global_defs.o one_d_relation_class.o xsect_classes.o reach_boundary_classes.o river_classes.o IO_util.o hecras_IO_b.f03
	$(COMPILER) -c hecras_IO_b.f03 

IO_util.o: global_defs.o IO_util.f03
	$(COMPILER) -c IO_util.f03

river_classes.o: global_defs.o one_d_relation_class.o xsect_classes.o reach_boundary_classes.o river_classes.f03
	$(COMPILER) -c river_classes.f03 

reach_boundary_classes.o: global_defs.o one_d_relation_class.o reach_boundary_classes.f03
	$(COMPILER) -c reach_boundary_classes.f03 

xsect_classes.o: global_defs.o one_d_relation_class.o xsect_classes.f03
	$(COMPILER) -c xsect_classes.f03 

one_d_relation_class.o : global_defs.o one_d_relation_class.f03
	$(COMPILER) -c one_d_relation_class.f03

global_defs.o: global_defs.f03
	$(COMPILER) -c $^


