all: qcov

qcov: force
	gnatmake -gnat05 -gnatwae -g qcov

clean: force
	$(RM) -f *.o *.ali qcov

force:
