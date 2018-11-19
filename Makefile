export FFLAGS = -Warn all -check all -g

deault: getoptf

getoptf: getoptf.f90
	$(FC) $(FFLAGS) -c getoptf.f90

tests: getoptf tests/
	$(MAKE) -C tests $(TG)

clean:
	rm -f getoptf *.mod *.o 	
	$(MAKE) -C tests clean
