GNATMAKE=gnatmake
ADAFLAGS=-gnat05 -gnatwae -gnata -g -gnatyg -Ignat -gnateS
PREFIX=install
RM=rm -f
CP=cp -pf
LN=ln -sf
MKDIR=mkdir -p
MAKEINFO=makeinfo
exeext=

# Binary program name
PGMNAME=gnatcov

# Directory name in the install tree
DIRNAME=gnatcoverage

all: $(PGMNAME)

$(PGMNAME): force
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_genopc:
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

GNAT_SOURCE_DIR=../gnat_src
ADA_GEN_SUBDIR=gnat
MOVE_IF_CHANGE=mv -f

copy_gnat_src: force
	@echo "GNAT sources are now mirrored in the GNATCOV repository iteslf"

#################
# Documentation #
#################

.PHONY: doc
doc: doc/$(PGMNAME).pdf doc/$(PGMNAME).info doc/$(PGMNAME).html doc/$(PGMNAME).txt

doc/$(PGMNAME).pdf: doc/$(PGMNAME).texi
	texi2pdf -b $<
	chmod o=u $(PGMNAME).pdf
	mv $(PGMNAME).pdf $@
	$(RM) *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

doc/$(PGMNAME).info: doc/$(PGMNAME).texi
	$(MAKEINFO) $< -o $@

doc/$(PGMNAME).html: doc/$(PGMNAME).texi
	$(MAKEINFO) --no-split --html -o $@ $< 

doc/$(PGMNAME).txt: doc/$(PGMNAME).texi
	$(MAKEINFO) --plaintext -o $@ $<

clean:
	$(RM) *.o *.ali $(PGMNAME) b~*

##########################
# Installation/Packaging #
##########################

.PHONY: install
install:
	$(MKDIR) $(PREFIX)/bin
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/html
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/pdf
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/info
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/txt
	$(CP) README $(PREFIX)
	$(CP) $(PGMNAME)$(exeext) $(PREFIX)/bin
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r doc/*.html $(PREFIX)/share/doc/$(DIRNAME)/html
	$(CP) -r doc/*.pdf  $(PREFIX)/share/doc/$(DIRNAME)/pdf
	$(CP) -r doc/*.txt  $(PREFIX)/share/doc/$(DIRNAME)/txt
	$(CP) -r doc/*.info $(PREFIX)/share/doc/$(DIRNAME)/info

force:
