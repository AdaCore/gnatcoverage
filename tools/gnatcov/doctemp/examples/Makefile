TARGET=powerpc-elf
RTS=zfp-prep

XCOVLEVEL=stmt
ANNOTATE=xcov

# unit test name and gpr file

UT=
GPR=ut.gpr
TDIR=tmp_$(UT)
OSUBDIR=obj

all:
	mkdir -p $(TDIR) && cp Makefile $(TDIR) && make -C $(TDIR) build run

build:
	@echo 'project UT is' > $(GPR)
	@echo '  for Languages use ("Ada");' >> $(GPR)
	@echo '  for Source_Dirs use ("../src");' >> $(GPR)
	@echo '  for Object_Dir use "$(OSUBDIR)";' >> $(GPR)
	@echo 'end UT;' >> $(GPR)
	gprbuild -p -f --target=$(TARGET) --RTS=$(TARGET)/$(RTS) -P$(GPR) $(UT).adb \
	-cargs:Ada -gnateS -gnaty -gnatwe -cargs -g -fpreserve-control-flow \
	-bargs -A=$(UT).alis

run:
	gnatcov run $(OSUBDIR)/$(UT)
	grep -v support.ali $(OSUBDIR)/$(UT).alis > alis
	gnatcov coverage --level=$(XCOVLEVEL) --scos=@alis \
	--annotate=$(ANNOTATE) $(UT).trace


