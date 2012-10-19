This code parses part of a hecras geometry file into some fortran classes for holding river data.

The reaches (names + coordinates + cross-sections (cutlines + profile + manning) ) are all read in

The main program is:

0) read_file.f95

Other code is:
1) global_defs.f95 -- global constants (e.g real precision, character length, etc)
2) river_classes.f95 -- classes for river / xsectional data
3) IO_util.f95 -- code to read files / manipulate strings
3) hecras_IO_b.f95 -- code to parse the hecras input data

4) makefile

5) hectest.g05 -- this is the hecras geometry file that I am using to develop the code. Reasonably complex
