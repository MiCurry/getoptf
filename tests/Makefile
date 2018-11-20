LINK=

ifeq ($(FC), ifort)
	LINK= -module ../
else
	LINK= -I ../
endif

tests: test_options

test_options: unittestf test_options.f90 ../getoptf.o unittestf.o
	$(FC) $(FFLAGS) $(LINK) -o ../test_options test_options.f90 unittestf.o ../getoptf.o

test_arguments: unittestf test_arguments.f90 ../getoptf.o unittestf.o
	$(FC) $(FFLAGS) $(LINK) ../test_arguments.f90 -o test_arguments unittestf.o ../getoptf.o

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
