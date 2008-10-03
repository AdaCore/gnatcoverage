GNATMAKE=gnatmake

all: xcov

xcov: force
	$(GNATMAKE) -gnat05 -gnatwae -gnata -g $@

ppc_genopc: force
	$(GNATMAKE) -gnat05 -gnatwae -gnata -g $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

clean: force
	$(RM) -f *.o *.ali xcov

force:
