This code parses part of a hecras geometry file into some fortran classes for holding river data. It's a bit of an excuse for me to learn some fortran file IO + OOP + think about how to set up a river network model, which I may one day populate with a solver

The reaches (names + coordinates + cross-sections (cutlines + profile + manning) ) are all read in.

The main program is:

0) main.f03

Other code is:
1) global_defs.f03 -- global constants (e.g real precision, character length, etc)
2) river_classes.f03 -- classes for river reach data
2b) xsect_classes.f03 -- classes for xsectional data
2c) reach_boundary_classes -- classes for reach boundaries
3) IO_util.f03 -- code to read files / manipulate strings
3) hecras_IO_b.f03 -- code to parse the hecras input data

4) makefile

5) hectest.g05 -- this is the (test) hecras geometry file that I am using to develop the code. Reasonably complex

6) one_d_relation_classes -- classes for a useful 'interpolation function' type object. 
