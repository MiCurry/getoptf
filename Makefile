export FFLAGS=

ifeq ($(FC), ifort)
	FFLAGS = -Warn all -check all -g
else
	FFLAGS = -Wall -fcheck=all -g
endif

deault: getoptf

getoptf: getoptf.f90
	$(FC) $(FFLAGS) -c getoptf.f90

tests: getoptf tests/
	$(MAKE) -C tests $(TG)

test_run: getoptf tests/
	$(MAKE) -C tests run

clean:
	rm -f getoptf *.mod *.o 	
	$(MAKE) -C tests clean
