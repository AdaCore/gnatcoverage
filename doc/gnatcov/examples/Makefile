XCOVLEVEL=stmt
ANNOTATE=xcov

# unit test name and gpr file

UT=  #test_...
GPR=ut.gpr
TDIR=_$(UT:test_%=%)
OSUBDIR=obj

all:
	mkdir -p $(TDIR) && cp Makefile $(TDIR) && make -C $(TDIR) build run

build:
	@echo 'project UT is' > $(GPR)
	@echo '  for Languages use ("Ada");' >> $(GPR)
	@echo '  for Source_Dirs use ("../src");' >> $(GPR)
	@echo '  for Object_Dir use "$(OSUBDIR)";' >> $(GPR)
	@echo 'end UT;' >> $(GPR)
	gnatcov instrument -P$(GPR) --level=$(XCOVLEVEL) --dump-filename-simple \
	--units=$(UT) $(UT).adb
	gprbuild -p -f -P$(GPR) $(UT).adb --src-subdirs=gnatcov-instr \
	--implicit-with=gnatcov_rts.gpr

run:
	$(OSUBDIR)/$(UT)
	gnatcov coverage --level=$(XCOVLEVEL) -P$(GPR) \
	--annotate=$(ANNOTATE) --units=$(UT) $(UT).srctrace
