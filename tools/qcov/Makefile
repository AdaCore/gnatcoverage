all: qcov

qcov: force
	gnatmake -gnat05 -gnatwae qcov

clean: force
	$(RM) -f *.o *.ali qcov

force:
