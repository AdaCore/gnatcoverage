GNATMAKE=gnatmake
PREFIX=install
RM=rm -f
CP=cp -pf
LN=ln -sf
MKDIR=mkdir -p
RMDIR=-rmdir
MAKEINFO=makeinfo
exeext=
SUBDIRS = trace_adapters/nexus
.PHONY: all $(SUBDIRS)

# Binary program name
PGMNAME=gnatcov

# Directory name in the install tree
DIRNAME=gnatcoverage

DOCFORMATS=pdf html

all: $(PGMNAME) $(SUBDIRS)

$(PGMNAME): force
	$(GNATMAKE) -Pgnatcov $(EXTRA_ADAFLAGS) $@

$(SUBDIRS):
	$(MAKE) -C $@

ppc_genopc:
	$(GNATMAKE) -Pgnatcov $(EXTRA_ADAFLAGS) $@

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

# We need two kinds of targets for documentation:
# - One for developers that just builds the docs and leaves
#   everything in place (e.g. temporary sphinx build subdirs)
# - One for package production purposes that clears the
#   intermediate artifacts

# The 'doc' general target builds all the formats and cleans up the build
# trees afterwards. This is to be used for package construction.

# The 'doc.<format>' targets just builds a format and leaves artifacts
# around.

# In both cases the constructed user doc elements are copied
# to the doc/<format> subdir eventually.

UMAN_DOCDIR=doc/umanual

doc-build.%:
	make -C $(UMAN_DOCDIR) $*

doc-copy.%:
	make -C $(UMAN_DOCDIR) COPYDIR=$(shell pwd)/doc/$* copy.$*

.PHONY: doc

doc.%: doc-build.% doc-copy.%
	@echo $@ done

doc: $(foreach fmt, $(DOCFORMATS), doc.$(fmt)) clean-doc-build

###################
# General cleanup #
###################

.PHONY: clean-bin clean-doc-copy clean-doc-build clean

clean-bin:
	$(RM) *.o *.ali $(PGMNAME) b~*

clean-doc-copy.%:
	   $(RM) -r doc/$*

clean-doc-copy: $(foreach fmt, $(DOCFORMATS), clean-doc-copy.$(fmt))

clean-doc-build:
	make -C $(UMAN_DOCDIR) clean

clean: clean-bin clean-doc-build clean-doc-copy
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done

################
# Installation #
################

.PHONY: install-bin install-examples install-doc install

install-bin:
	$(MKDIR) $(PREFIX)/bin
	$(CP) $(PGMNAME)$(exeext) $(PREFIX)/bin

install-examples:
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

install-doc.%:
	   $(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/$*
	   $(CP) -r doc/$*/* $(PREFIX)/share/doc/$(DIRNAME)/$*

install-doc: $(foreach fmt, $(DOCFORMATS), install-doc.$(fmt))

#install: install-bin install-examples install-doc
install: install-bin install-examples
	for Dir in $(SUBDIRS); do \
	  $(MAKE) -C $$Dir PREFIX=$(PREFIX) install || exit 1; \
	done

#############
# Packaging #
#############

.PHONY: distrib

distrib: install
	$(CP) README INSTALL $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
            -e "/^version/s/..$$/\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall

force:
