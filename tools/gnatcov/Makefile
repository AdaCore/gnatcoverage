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

DOCFORMATS=pdf html
ALLDOCS=$(foreach fmt, $(DOCFORMATS), doc-$(fmt))

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

UMAN_DOCDIR=doc/umanual

.PHONY: doc
doc: $(ALLDOCS)

doc-prepare:
	cd $(UMAN_DOCDIR); \
	perl -pi -e "s/^version =.*/version = \"$$($(PGMNAME) --version)\"/" conf.py

doc-pdf: doc-prepare
	cd $(UMAN_DOCDIR); make pdf 

doc-html: doc-prepare
	cd $(UMAN_DOCDIR); make html

clean:
	$(RM) *.o *.ali $(PGMNAME) b~* $(ALLDOCS)

##########################
# Installation/Packaging #
##########################

.PHONY: install distrib
install:
	$(MKDIR) $(PREFIX)/bin
	$(CP) $(PGMNAME)$(exeext) $(PREFIX)/bin

	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/html
	$(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/pdf

	$(CP) -r $(UMAN_DOCDIR)/_build/html/*.html \
	   $(PREFIX)/share/doc/$(DIRNAME)/html
	$(CP) -r $(UMAN_DOCDIR)/_build/pdf/*.pdf \
	   $(PREFIX)/share/doc/$(DIRNAME)/pdf

distrib: install
	$(CP) README INSTALL $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall
force:
