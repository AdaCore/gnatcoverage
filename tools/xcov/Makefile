GNATMAKE=gnatmake

all: xcov

xcov: force
	$(GNATMAKE) -gnat05 -gnatwae -g xcov

clean: force
	$(RM) -f *.o *.ali xcov

force:
