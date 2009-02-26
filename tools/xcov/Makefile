GNATMAKE=gnatmake
ADAFLAGS=-gnat05 -gnatwae -gnata -g -gnatyg-s
PREFIX=install
RM=rm -f
CP=cp -p
MKDIR=mkdir -p
MAKEINFO=makeinfo

all: xcov

xcov: force
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_genopc:
	$(GNATMAKE) $(ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

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
