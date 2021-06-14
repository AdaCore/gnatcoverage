###############################################
# Targets and subtargets of interest to users #
###############################################

# Documentation is now handled in a parent subdir, as we get
# more tightly integrated with gnattest tool.

# ----------------------------------
# -- To build the excutable stuff --
# ----------------------------------
# (requires a Ada 2012 capable gnat toolchain + gnatcoll)
#
# all
#   bin        <- default
#   adapters [ADAPTERS_LIST=dynamorio|valgrind|...]

# ----------------
# -- To install --
# ----------------
#
# install
#   install-bin
#   install-examples
#
# install-adapters [ADAPTERS_LIST=dynamorio|valgrind|...]
#
# All working with the PREFIX variable, that users may specify:

# Base installation dir. Should be absolute pathname. Beware of
# possible conflicts with the INSTALL text file of our distribution,
# on possibly case-insensitive filesystems.
PREFIX=$(PWD)/local

# By default, assume that the binutils dependency was built in the source tree.
BINUTILS_BUILD_DIR?=$(BINUTILS_SRC_DIR)

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
#   clean-adapters

###################
# Internal macros #
###################

GPRBUILD=gprbuild
GPRINSTALL=gprinstall

# Binary program name
PGMNAME=gnatcov
PGMPATH=obj64/$(BUILD_MODE)/$(PGMNAME)
PGM32PATH=obj32/$(BUILD_MODE)/gnatcov32
PGM64PATH=obj64/$(BUILD_MODE)/gnatcov64

# Directory name in the install tree
DIRNAME=gnatcoverage

include host.mk
include common.mk

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
DEFAULT_ADAPTERS = nexus dynamorio trace32
else
DEFAULT_ADAPTERS = dynamorio
endif
endif

ifeq ($(HOST_OS), linux)
ifeq ($(BITS_ARCH), 32)
DEFAULT_ADAPTERS = valgrind trace32
else
DEFAULT_ADAPTERS = valgrind
endif
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

# Trace32 and Nexus (iSystem) adapters use the same prefix as GNATcov
trace32_ADAPTER_INSTALL_VARS=PREFIX=$(PREFIX)
nexus_ADAPTER_INSTALL_VARS=PREFIX=$(PREFIX)

.PHONY: default all adapters bin $(PGMPATH)

default: bin
all: bin adapters

bin:
# Unlike BINUTILS_BUILD_DIR for which we have a default fallback,
# BINUTILS_SRC_DIR must be defined explicitely.
	@if [ -z "$(BINUTILS_SRC_DIR)" ]; then \
		echo "Please define the BINUTILS_SRC_DIR and" \
		     " BINUTILS_BUILD_DIR variables to point to the binutils" \
		     " source and build directories."; \
		exit 1; \
	fi

# Use "set -e" not to start the second build if the first one failed. Also
# start with the 64-bit one so that errors in the "gnatcov" main closure show
# up as soon as possible.
	set -e && for bits in 64 32; \
	do \
	  $(GPRBUILD) -Pgnatcov -p -j$(PARALLEL_BUILD) \
	    -XARCH=$$bits \
	    -XBUILD_MODE=$(BUILD_MODE) \
	    -XBINUTILS_SRC_DIR="$(BINUTILS_SRC_DIR)" \
	    -XBINUTILS_BUILD_DIR="$(BINUTILS_BUILD_DIR)" \
	    $(EXTRA_ADAFLAGS) \
	    -largs $(LD_FLAGS) \
	    -cargs:c++ $(CXXFLAGS); \
	done

ADAPTER_TARGETS=$(foreach a, $(ADAPTERS_LIST), adapter-$(a))
.PHONY: $(ADAPTERS_TARGETS)

# Some adapters reuse part of the gnatcov sources and we need to prevent
# concurrent builds of the gnatcov part from "all", via "bin" and via
# "adapters". Simplest is to make sure that "bin" is done before any of
# the "adapters" targets may start:

$(ADAPTER_TARGETS): adapter-%:
	$(MAKE) -C trace_adapters/$* $($*_ADAPTER_BUILD_VARS)

adapters: $(ADAPTER_TARGETS)

#######################
# Internal facilities #
#######################

ppc_genopc:
	$(GPRBUILD) -j$(PARALLEL_BUILD) -Pgnatcov $(EXTRA_ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

###################
# General cleanup #
###################

.PHONY: clean-bin clean clean-adapters

clean-bin:
	$(RM) *.o *.ali $(PGMNAME) b~* obj*/*

clean: clean-bin clean-adapters

CLEAN_ADAPTER_TARGETS=$(foreach a, $(ADAPTERS_LIST), clean-adapter-$(a))
.PHONY: $(CLEAN_ADAPTER_TARGETS)

$(CLEAN_ADAPTER_TARGETS): clean-adapter-%:
	$(MAKE) -C trace_adapters/$* clean

clean-adapters: $(CLEAN_ADAPTER_TARGETS)

################
# Installation #
################

.PHONY: install-bin install-examples install install-adapters \
	install-gnatcov_rts install-lib

install-bin:
	$(MKDIR) $(PREFIX)/bin
	$(CP) $(PGMPATH)$(exeext) $(PREFIX)/bin/

	$(MKDIR) $(PREFIX)/libexec/gnatcoverage
	$(CP) \
	   $(PGM32PATH)$(exeext) \
	   $(PGM64PATH)$(exeext) \
	   $(PREFIX)/libexec/gnatcoverage

install-examples:
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

install-gnatcov_rts:
	$(RM) -r $(PREFIX)/share/$(DIRNAME)/gnatcov_rts
	$(MKDIR) $(PREFIX)/share/$(DIRNAME)/gnatcov_rts
	$(CP) -r rts/*.gpr rts/*.ad* rts/*.c rts/*.h $(PREFIX)/share/$(DIRNAME)/gnatcov_rts

install-lib:
	$(MKDIR) $(PREFIX)/lib/gnatcoverage
	$(CP) gnatcov-xml-report.xsd $(PREFIX)/lib/gnatcoverage

install-common: install-bin install-examples install-gnatcov_rts install-lib
install-without-doc: install-common
install: install-without-doc

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
# We need to add this script, README, INSTALL and COPYING3/LICENSE.dhtml files
# in this case.

# Windows distribs are packaged and installed by an nsis installer or alike,
# which has its own extra info embedded and eventually installs just all what
# the distrib tree contains. We don't want any toplevel item there.

.PHONY: distrib-linux-extra distrib-windows-extra distrib

distrib-linux-extra:
	[ -f COPYING3 ] || $(CP) ../../COPYING3 .
	$(CP) README.md INSTALL COPYING3 LICENSE.dhtml $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall

distrib-windows-extra:

distrib-common: install-adapters distrib-${HOST_OS}-extra
distrib-without-doc: distrib-common install-without-doc
distrib: distrib-without-doc

#############
# GNAT_UTIL #
#############

# To support build from sources with toolchains which don't
# provide gnat_util:

# The name of the subdir where we will setup the gnat_util
# source files, which should match the source dir designated
# by gnat_util.gpr:
GU_SUBDIR=gnat_util-src

.PHONY: gnat_util gnat_util_src gnat_util.gpr

# The name of the target referenced in the README file:
gnat_util: gnat_util_src gnat_util.gpr

gnat_util_src:
	@if [ -z "$(GNAT_SRC_DIR)" ]; then \
		echo "!! Please define the GNAT_SRC_DIR variable !!"; \
	fi
	$(MKDIR) $(GU_SUBDIR)
	for src in $$(cat $(GNAT_SRC_DIR)/MANIFEST.gnat_util); do \
	  $(CP) $(GNAT_SRC_DIR)/$$src $(GU_SUBDIR); \
	done
	(cd $(GU_SUBDIR) && \
           gnatmake -gnatf -gnatwae -gnatyg -gnatyS xsnamest && \
           ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)
	$(CP) $(GNAT_SRC_DIR)/sdefault_adb.gnat_util $(GU_SUBDIR)/sdefault.adb

gnat_util.gpr:
	@echo "project gnat_util is" > $@
	@echo "  for source_dirs use (\"$(GU_SUBDIR)\");" >> $@
	@echo "end gnat_util;" >> $@

.PHONY: force

force:
