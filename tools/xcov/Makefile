GNATMAKE=gnatmake
ADAFLAGS=-gnat05 -gnatwae -gnata -g -gnatyg-s
all: xcov

xcov: force
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_genopc: force
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

clean: force
	$(RM) -f *.o *.ali xcov

force:
