test_options: unittestf test_options.f90 ../getoptf.o unittestf.o
	$(FC) $(FFLAGS) -module ../ test_options.f90 -o test_options unittestf.o ../getoptf.o

test_arguments: unittestf test_arguments.f90 ../getoptf.o unittestf.o
	$(FC) $(FFLAGS) -module ../ test_arguments.f90 -o test_arguments unittestf.o ../getoptf.o

run: test_options
	./test_options 

# Modules
unittestf: unittestf.f90
	$(FC) $(FFLAGS) -c unittestf.f90

clean:
	rm -f *.mod *.o test_options


# Other test options
#test_errors.f90
#test_opterr.f90
#test_optintd.f90
#test_optopt.f90
#unittestf.f90
