###############################################
# Targets and subtargets of interest to users #
###############################################

# ----------------------------------
# -- To build the excutable stuff --
# ----------------------------------
# (requires a Ada 2012 capable gnat toolchain + gnatcoll)
#
# all
#   bin        <- default
#   adapters

# --------------------------------
# -- To build the documentation --
# --------------------------------
# (requires sphinx & latex + tools)
#
# doc
#   doc.pdf 
#   doc.html

# ----------------
# -- To install --
# ----------------
#
# install
#   install-bin
#   install-examples
#   install-doc
#
# install-adapters
#
# All working with the PREFIX variable, that users may specify:

# Base installation dir. Should be absolute pathname.
PREFIX=$(PWD)/install

# Note for the valgrind adapter on linux:
# ---------------------------------------
# By default, install-adapters installs the valgrind coverage plugin in
# the bundled-in valgrind tree (in <PREFIX>/libexec/gnatcoverage), populated
# within binary packages only.
#
# When there is no such tree available (e.g. building gnatcov entirely from
# sources), gnatcov will rely on PATH to find a suitable valgrind and the
# adapter must be installed in the corresponding lib subdir. This is requested
# by providing an explicit VALGRIND_PREFIX value.

# ----------------
# -- To cleanup --
# ----------------
#
# clean
#   clean-bin
#   clean-doc
#   clean-adapters

###################
# Internal macros #
###################

GNATMAKE=gnatmake
RM=rm -f
CP=cp -pf
MKDIR=mkdir -p
exeext=

# Binary program name
PGMNAME=gnatcov

# Directory name in the install tree
DIRNAME=gnatcoverage

# Documentation formats to produce on "make doc"
DOCFORMATS=pdf html

############################
# Kind of HOST OS we're on #
############################

UNAME=$(shell uname -s)

ifneq (,$(filter MINGW% CYGW%, $(UNAME)))
HOST_OS=windows
endif
ifneq (,$(filter Linux, $(UNAME)))
HOST_OS=linux
endif

#####################################################################
# Targets to build the core tool and the trace adapters, optionally #
#####################################################################

# We provide distinct targets for the two categories of artifacts so
# users get the core only by default in case they don't need the
# adapters for their host, e.g. when building for gnatemu targets.

# For example, a user who needs to build gnatcov on linux to perform
# ppc-elf coverage only doesn't need the valgrind adapter for native
# coverage. He might moreover be on a system where the adapter build
# would fail (e.g. missing header files), so we really shouldn't try
# to build these adapters by default.

# So ...
#
# make or make bin
#
#   builds the core gnatcov tools only
#
# make all or make bin adapters
#
#   builds both the core tools and the adapters of
#   relevance for the host

# Determine the set of ADAPTER_SUBDIRS of relevance on our host OS

ADAPTER_SUBDIRS =
ifeq ($(HOST_OS), windows)
ADAPTER_SUBDIRS = trace_adapters/nexus
endif
ifeq ($(HOST_OS), linux)
ADAPTER_SUBDIRS = trace_adapters/valgrind
endif

PARALLEL_BUILD=8

.PHONY: default all adapters bin $(PGMNAME) $(ADAPTER_SUBDIRS)

default: bin

adapters: $(ADAPTER_SUBDIRS)

bin: $(PGMNAME)

all: bin adapters

$(PGMNAME):
	$(GNATMAKE) -j$(PARALLEL_BUILD) -Pgnatcov $(EXTRA_ADAFLAGS) $@

$(ADAPTER_SUBDIRS):
	$(MAKE) -C $@


#######################
# Internal facilities #
#######################

ppc_genopc:
	$(GNATMAKE) -j$(PARALLEL_BUILD) -Pgnatcov $(EXTRA_ADAFLAGS) $@

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

.PHONY: clean-bin clean-doc clean-doc-copy clean-doc-build clean clean-adapters

clean-bin:
	$(RM) *.o *.ali $(PGMNAME) b~*

clean-doc-copy.%:
	   $(RM) -r doc/$*

clean-doc-copy: $(foreach fmt, $(DOCFORMATS), clean-doc-copy.$(fmt))

clean-doc-build:
	make -C $(UMAN_DOCDIR) clean

clean-adapters:
	for dir in $(ADAPTER_SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done

clean-doc: clean-doc-build clean-doc-copy

clean: clean-bin clean-doc clean-adapters

################
# Installation #
################

.PHONY: install-bin install-examples install-doc install install-adapters

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

install: install-bin install-examples install-doc

install-adapters:
	for dir in $(ADAPTER_SUBDIRS); do \
	  $(MAKE) -C $$dir \
               PREFIX=$(PREFIX) \
               $(INSTALL_ADAPTERS_VARS_$(HOST_OS)) \
               install || exit 1; \
	done

# For linux adapters, provide a default VALGRIND_PREFIX to the valgrind
# plugin installer, designating our local libexec subdir:

INSTALL_ADAPTERS_VARS_linux = VALGRIND_PREFIX?=$(PREFIX)/libexec/gnatcoverage
INSTALL_ADAPTERS_VARS_windows = 

#############
# Packaging #
#############

# Prepare a to-be packaged distribution tree

# Unix distribs are tar or zip versions of the three main items (bin, doc
# and examples), unpacked by users then installed with a doinstall script.
# We need to add this script, a README and an INSTALL file in this case.

# Windows distribs are packaged and installed by an nsis installer or alike,
# which has it's own extra info embedded and eventually installs just all what
# the distrib tree contains. We don't want any toplevel item there.

.PHONY: distrib-linux-extra distrib-windows-extra distrib

distrib-linux-extra:
	$(CP) README INSTALL $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall

distrib-windows-extra:

distrib: install install-adapters distrib-${HOST_OS}-extra

.PHONY: force

force:
