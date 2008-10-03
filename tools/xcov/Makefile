GNATMAKE=gnatmake

all: xcov

xcov: force
	$(GNATMAKE) -gnat05 -gnatwae -gnata -g $@

ppc_genopc: force
	$(GNATMAKE) -gnat05 -gnatwae -gnata -g $@

clean: force
	$(RM) -f *.o *.ali xcov

force:
