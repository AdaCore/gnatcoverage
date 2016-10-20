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
#   adapters [ADAPTERS_LIST=dynamorio|valgrind|...]

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
# install-adapters [ADAPTERS_LIST=dynamorio|valgrind|...]
#
# All working with the PREFIX variable, that users may specify:

# Base installation dir. Should be absolute pathname. Beware of
# possible conflicts with the INSTALL text file of our distribution,
# on possibly case-insensitive filesystems.
PREFIX=$(PWD)/local

# By default, assume that the binutils dependency was built in the source tree.
BINUTILS_BUILD_DIR=$(BINUTILS_SRC_DIR)

# Note for the trace adapters:
# ----------------------------

# When *building* adapters for native configurations, we usually need to know
# the execution engine "prefix" to get access to e.g. header files (valgrind)
# or cmake artifacts (dynamoRIO).

# By default, we infer that prefix from the location of "valgrind" or "drrun"
# on the PATH, and users may specify an alternate location by setting the
# VALGRIND_PREFIX or DYNRIO_PREFIX Makefile variable.

# When *installing* adapters, we need the location of the valgrind install
# tree where the valgrind adapter needs to go. By default, we infer that from
# the location of "valgrind" on the PATH.  Users may alter that choice by
# setting the VALGRIND_PREFIX variable.

# dynamorio doesn't need the client to be installed within it's own tree, so
# we install within the bundled install tree, rooted at PREFIX and gnatcov
# passes the complete client path to dynamorio.

# Alternatively, users may specify a final location by setting the
# VALGRIND_TOOL_INSTALL_DIR or DYNRIO_CLIENT_INSTALL_DIR variable.

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

GPRBUILD=gprbuild

# Binary program name
PGMNAME=gnatcov
PGMPATH=obj$(BITS_ARCH)/$(PGMNAME)

# Directory name in the install tree
DIRNAME=gnatcoverage

# Documentation formats to produce on "make doc"
DOCFORMATS=pdf html

include host.mk

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

# The set of trace adapters to consider.  Users may pass an explicit
# list (whitespace separated list of subdirs within trace_adapters/).

include architecture.mk

ifeq ($(HOST_OS), windows)
ifeq ($(BITS_ARCH), 32)
DEFAULT_ADAPTERS = nexus dynamorio
else
DEFAULT_ADAPTERS = dynamorio
endif
endif
ifeq ($(HOST_OS), linux)
DEFAULT_ADAPTERS = valgrind
endif

ADAPTERS_LIST=$(DEFAULT_ADAPTERS)

PARALLEL_BUILD=0
BUILD_MODE=dev

# ================================================================
# == Variables to pass down to sub-makefiles for trace adapters ==
# ================================================================

# For valgrind, we need a prefix indication at build time to find header
# files, and at install time to place the "tool" within the valgrind install
# tree.

VALGRIND_PREFIX?=$(shell dirname `which valgrind`)/..

valgrind_ADAPTER_BUILD_VARS=\
  VALGRIND_PREFIX?=$(VALGRIND_PREFIX)

valgrind_ADAPTER_INSTALL_VARS=\
  VALGRIND_TOOL_INSTALL_DIR?=$(VALGRIND_PREFIX)/lib/valgrind

# For dynamorio, we need a prefix indication at build time to find header
# files and cmake artifacts, and can always install within the gnatcov install
# tree.

DYNRIO_PREFIX?=$(shell dirname `which drrun`)/..

dynamorio_ADAPTER_BUILD_VARS=\
  DYNRIO_PREFIX?=$(DYNRIO_PREFIX)

dynamorio_ADAPTER_INSTALL_VARS=\
  DYNRIO_CLIENT_INSTALL_DIR?=$(PREFIX)/libexec/gnatcoverage/lib$(BITS_ARCH)

.PHONY: default all adapters bin $(PGMPATH)

default: bin

bin: $(PGMPATH)

all: bin adapters

$(PGMPATH):
	# Shortcut for developpers: if there is no BUILD_DIR, assume it's the
	# same as SRC_DIR.
	if [ -z "$(BINUTILS_SRC_DIR)" ]; then \
		echo "Please define the BINUTILS_SRC_DIR and" \
		     " BINUTILS_BUILD_DIR variables to point to the binutils" \
		     " source and build directories."; \
		exit 1; \
	fi
	$(GPRBUILD) -Pgnatcov -p -j$(PARALLEL_BUILD) \
	-XARCH=$(BITS_ARCH) -XBUILD_MODE=$(BUILD_MODE) \
	-XBINUTILS_SRC_DIR="$(BINUTILS_SRC_DIR)" \
	-XBINUTILS_BUILD_DIR="$(BINUTILS_BUILD_DIR)" \
	$(EXTRA_ADAFLAGS) $(PGMNAME)

ADAPTER_TARGETS=$(foreach a, $(ADAPTERS_LIST), adapter-$(a))
.PHONY: $(ADAPTERS_TARGETS)

# Some adapters reuse part of the gnatcov sources and we need to prevent
# concurrent builds of the gnatcov part from "all", via "bin" and via
# "adapters". Simplest is to make sure that "bin" is done before any of
# the "adapters" targets may start:

$(ADAPTER_TARGETS): adapter-%: bin
	$(MAKE) -C trace_adapters/$* $($*_ADAPTER_BUILD_VARS)

adapters: $(ADAPTER_TARGETS)

#######################
# Internal facilities #
#######################

ppc_genopc:
	$(GPRBUILD) -j$(PARALLEL_BUILD) -Pgnatcov $(EXTRA_ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

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
	make -C $(UMAN_DOCDIR) BUILD_MODE=$(BUILD_MODE) $*

doc-copy.%:
	make -C $(UMAN_DOCDIR) BUILD_MODE=$(BUILD_MODE) COPYDIR=$(shell pwd)/doc/$* copy.$*

.PHONY: doc

doc.%: doc-build.% doc-copy.%
	@echo $@ done

doc: $(foreach fmt, $(DOCFORMATS), doc.$(fmt)) clean-doc-build

###################
# General cleanup #
###################

.PHONY: clean-bin clean-doc clean-doc-copy clean-doc-build clean clean-adapters

clean-bin:
	$(RM) *.o *.ali $(PGMNAME) b~* obj*/*

clean-doc-copy.%:
	   $(RM) -r doc/$*

clean-doc-copy: $(foreach fmt, $(DOCFORMATS), clean-doc-copy.$(fmt))

clean-doc-build:
	make -C $(UMAN_DOCDIR) clean

clean-doc: clean-doc-build clean-doc-copy

clean: clean-bin clean-doc clean-adapters

CLEAN_ADAPTER_TARGETS=$(foreach a, $(ADAPTERS_LIST), clean-adapter-$(a))
.PHONY: $(CLEAN_ADAPTER_TARGETS)

$(CLEAN_ADAPTER_TARGETS): clean-adapter-%:
	$(MAKE) -C trace_adapters/$* clean

clean-adapters: $(CLEAN_ADAPTER_TARGETS)

################
# Installation #
################

.PHONY: install-bin install-examples install-doc install install-adapters

install-bin:
	$(MKDIR) $(PREFIX)/bin
	$(CP) $(PGMPATH)$(exeext) $(PREFIX)/bin/$(PGMNAME)$(BITS_ARCH)$(exeext)
	cd $(PREFIX)/bin && $(RM) $(PGMNAME)$(exeext) && $(LN_S) $(PGMNAME)$(BITS_ARCH)$(exeext) $(PGMNAME)$(exeext)

install-examples:
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

install-doc.%:
	   $(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/$*
	   $(CP) -r doc/$*/* $(PREFIX)/share/doc/$(DIRNAME)/$*

install-doc: $(foreach fmt, $(DOCFORMATS), install-doc.$(fmt))

install-common: install-bin install-examples
install-without-doc: install-common
install-with-doc: install-common install-doc
install: install-with-doc

INSTALL_ADAPTER_TARGETS=$(foreach a, $(ADAPTERS_LIST), install-adapter-$(a))
.PHONY: $(INSTALL_ADAPTER_TARGETS)

$(INSTALL_ADAPTER_TARGETS): install-adapter-%:
	$(MAKE) -C trace_adapters/$* $($*_ADAPTER_INSTALL_VARS) install

install-adapters: $(INSTALL_ADAPTER_TARGETS)
	# Make sure we have the standard libexec subdir, even if
	# the list of adapters is empty
	$(MKDIR) $(PREFIX)/libexec

#############
# Packaging #
#############

# Prepare a to-be packaged distribution tree

# Unix distribs are tar or zip versions of the three main items (bin, doc
# and examples), unpacked by users then installed with a doinstall script.
# We need to add this script, README, INSTALL and LICENSE files in this case.

# Windows distribs are packaged and installed by an nsis installer or alike,
# which has its own extra info embedded and eventually installs just all what
# the distrib tree contains. We don't want any toplevel item there.

.PHONY: distrib-linux-extra distrib-windows-extra distrib

distrib-linux-extra:
	$(CP) README.md INSTALL LICENSE $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall

distrib-windows-extra:

distrib-common: install-adapters distrib-${HOST_OS}-extra
distrib-without-doc: distrib-common install-without-doc
distrib-with-doc: distrib-common install-with-doc
distrib: distrib-with-doc

.PHONY: force

force:
