
$(COMPILER) 
$(FFLAGS)


all:

# Tests
test_options: test_options.f90 getoptf.o unittest.o
	$(COMPILER) $(FFLAGS) test_options.f90 -o test_options unittest.o

test_arguments: test_arguments.f90 getoptf.o unittest.o
	$(COMPILER) $(FFLAGS) test_arguments.f90 -o test_arguments unittest.o


# Modules

unittestf: unittestf.f90
	$(COMPILER) $(FFLAGS) -c unittest.f90

getoptf: getoptf.f90
	$(COMPILER) $(FFLAGS) -c getoptf.f90


# Other test options
#test_errors.f90
#test_opterr.f90
#test_optintd.f90
#test_optopt.f90
#unittestf.f90
