all: xcov

xcov: force
	gnatmake -gnat05 -gnatwae -g xcov

clean: force
	$(RM) -f *.o *.ali xcov

force:
