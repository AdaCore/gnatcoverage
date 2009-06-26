GNATMAKE=gnatmake
ADAFLAGS=-gnat05 -gnatwae -gnata -g -gnatyg -Ignat
PREFIX=install
RM=rm -f
CP=cp -p
LN=ln -sf
MKDIR=mkdir -p
MAKEINFO=makeinfo

all: xcov

xcov: force
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_genopc:
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

GNAT_SOURCE_DIR=../gnat_src
ADA_GEN_SUBDIR=gnat
MOVE_IF_CHANGE=mv -f
-include gnat/Make-generated.in

copy_gnat_src: force
	for f in `cat $(ADA_GEN_SUBDIR)/MANIFEST.XCOV`; do \
	$(LN) $(GNAT_SOURCE_DIR)/$$f $(ADA_GEN_SUBDIR)/$$f; done

#################
# Documentation #
#################

.PHONY: doc
doc: doc/xcov.pdf doc/xcov.info doc/xcov.html doc/xcov.txt

doc/xcov.pdf: doc/xcov.texi
	texi2pdf -b $<
	mv xcov.pdf $@
	$(RM) *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

doc/xcov.info: doc/xcov.texi
	$(MAKEINFO) $< -o $@

doc/xcov.html: doc/xcov.texi
	$(MAKEINFO) --no-split --html -o $@ $< 

doc/xcov.txt: doc/xcov.texi
	$(MAKEINFO) --plaintext -o $@ $<

clean:
	$(RM) *.o *.ali xcov b~*

##########################
# Installation/Packaging #
##########################

install:
	$(MKDIR) $(PREFIX)/bin
	$(MKDIR) $(PREFIX)/share/examples/xcov
	$(MKDIR) $(PREFIX)/share/doc/xcov
	$(MKDIR) $(PREFIX)/share/doc/xcov/html
	$(MKDIR) $(PREFIX)/share/doc/xcov/pdf
	$(MKDIR) $(PREFIX)/share/doc/xcov/info
	$(MKDIR) $(PREFIX)/share/doc/xcov/txt
	$(CP) README $(PREFIX)
	$(CP) xcov $(PREFIX)/bin
	$(CP) -r examples/* $(PREFIX)/share/examples/xcov
	$(CP) -r doc/*.html $(PREFIX)/share/doc/xcov/html
	$(CP) -r doc/*.pdf  $(PREFIX)/share/doc/xcov/pdf
	$(CP) -r doc/*.txt  $(PREFIX)/share/doc/xcov/txt
	$(CP) -r doc/*.info $(PREFIX)/share/doc/xcov/info

force:
