all: qcov

qcov: force
	gnatmake -gnat05 qcov

clean: force
	$(RM) -f *.o *.ali qcov

force:
