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
#   adapters [ADAPTERS_LIST=trace32|...]

# ----------------
# -- To install --
# ----------------
#
# install
#   install-bin
#   install-examples
#
# install-adapters [ADAPTERS_LIST=trace32|...]
#
# All working with the PREFIX variable, that users may specify:

# Base installation dir. Should be absolute pathname. Beware of
# possible conflicts with the INSTALL text file of our distribution,
# on possibly case-insensitive filesystems.
PREFIX=$(PWD)/local

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
PGMPATH=obj32/$(BUILD_MODE)/$(PGMNAME)
PGM32PATH=obj32/$(BUILD_MODE)/gnatcov32
PGM64PATH=obj64/$(BUILD_MODE)/gnatcov64
COMPILERPATH=obj32/$(BUILD_MODE)/compiler_wrappers-gcc

# Directory name in the install tree
DIRNAME=gnatcoverage

include host.mk

# We want to allow community users to build gnatcov without a dependency to a
# customized version of libclang (needed for C instrumentation), through the
# variable C_SUPPORT.
#
# This requires not passing the Clang / LLVM compiler flags, that are passed
# through libclang_common.mk, and stubbing the libclang project file (done here)
# and the Instrument.C unit in gnatcov (done in gnatcov.gpr).
C_SUPPORT?=True
ifeq ($(C_SUPPORT), True)
include libclang_common.mk
else
CURRENT_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
GPR_PROJECT_PATH := $(CURRENT_DIR)/gpr_stubs:$(GPR_PROJECT_PATH)
endif

#####################################################################
# Targets to build the core tool and the trace adapters, optionally #
#####################################################################

# We provide distinct targets for the two categories of artifacts so
# users get the core only by default in case they don't need the
# adapters for their host, e.g. when building for gnatemu targets.

# For example, a user who needs to build gnatcov on linux to perform ppc-elf
# coverage only doesn't need the trace32 adapter for the corresponding probe.
# He might moreover be on a system where the adapter build would fail (e.g.
# missing probe-specific files), so we really shouldn't try to build these
# adapters by default.

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
DEFAULT_ADAPTERS = trace32 llvm_json
else
DEFAULT_ADAPTERS =
endif
endif

ifeq ($(HOST_OS), linux)
ifeq ($(BITS_ARCH), 32)
DEFAULT_ADAPTERS =
else
DEFAULT_ADAPTERS = trace32 llvm_json
endif
endif

ADAPTERS_LIST ?= $(DEFAULT_ADAPTERS)

PROCESSORS=0
BUILD_MODE=dev

# ================================================================
# == Variables to pass down to sub-makefiles for trace adapters ==
# ================================================================

# The trace adapters use the same prefix as GNATcov
trace32_ADAPTER_INSTALL_VARS=PREFIX=$(PREFIX)
llvm_json_ADAPTER_INSTALL_VARS=PREFIX=$(PREFIX)

.PHONY: default all adapters bin $(PGMPATH)

default: bin
all: bin adapters

# Here we build gnatcov32, gnatcov64 and the "gnatcov" driver that spawns
# one or the other of the first two depending on the operation to perform
# and the kind of binary item at hand, if any. Source instrumentation is
# unconditionally directed to gnatcov64, so this is the only program where
# "instrument" support is needed.

BIN_COMMON_BUILD_ARGS=\
 -Pgnatcov.gpr -p -j$(PROCESSORS) \
 -XBUILD_MODE=$(BUILD_MODE) \
 -XC_SUPPORT=$(C_SUPPORT) \
 $(EXTRA_ADAFLAGS) \
 -largs $(LD_FLAGS) \
 -cargs:c++ $(CXXFLAGS) \
 -gargs
INSTR_COMMON_ARGS=\
  -j$(PROCESSORS) \
  -P gnatcov.gpr \
  --c++-opts=$(CXXFLAGS)
INSTR_BIN_BUILD_ARGS=\
  $(BIN_COMMON_BUILD_ARGS) \
  --src-subdirs=gnatcov-instr \
  --implicit-with=gnatcov_rts

bin:
ifdef INSTRUMENTED
	# We need to instrument and build at the same time: the driver and
	# gnatcov32 builds share the same object directory, meaning that
	# gnatcov will remove files resulting from the instrumentation of
	# gnatcov32 when instrumenting the driver. This will be a problem: in
	# the former case, gnatcov_bits_specific.adb is instrumented as a main
	# but not in the latter case (gnatcov.adb is). This means that if we
	# use the version that was not instrumented as a main to compile
	# gnatcov32, running gnatcov32 will not dump execution traces.

	gnatcov instrument $(INSTR_COMMON_ARGS) -XPART=gnatcov64
	$(GPRBUILD) $(INSTR_BIN_BUILD_ARGS) -XPART=gnatcov64

	gnatcov instrument $(INSTR_COMMON_ARGS) -XPART=gnatcov32
	$(GPRBUILD) $(INSTR_BIN_BUILD_ARGS) -XPART=gnatcov32

	gnatcov instrument $(INSTR_COMMON_ARGS) -XPART=driver
	$(GPRBUILD) $(INSTR_BIN_BUILD_ARGS) -XPART=driver

	gnatcov instrument $(INSTR_COMMON_ARGS) -XPART=compiler
	$(GPRBUILD) $(INSTR_BIN_BUILD_ARGS) -XPART=compiler
else
	# Start with the 64-bit gnatcov, which has the largest closure
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=gnatcov64
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=gnatcov32
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=driver
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=compiler
endif

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
	$(GPRBUILD) -j$(PROCESSORS) -Pgnatcov $(EXTRA_ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

###################
# General cleanup #
###################

.PHONY: clean-bin clean clean-adapters

clean-bin:
	$(RM) -rf *.o *.ali $(PGMNAME) b~* obj*/

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
	   $(COMPILERPATH)$(exeext) \
	   $(PREFIX)/libexec/gnatcoverage

install-examples:
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

install-gnatcov_rts:
	$(RM) -r $(PREFIX)/share/$(DIRNAME)/gnatcov_rts
	$(MKDIR) $(PREFIX)/share/$(DIRNAME)/gnatcov_rts
	$(RM) -r $(PREFIX)/share/$(DIRNAME)/gnatcov_ada_rts
	$(MKDIR) $(PREFIX)/share/$(DIRNAME)/gnatcov_ada_rts
	$(CP) -r rts/*.gpr rts/*.ads rts/*.adb rts/*.c rts/*.h \
	   $(PREFIX)/share/$(DIRNAME)/gnatcov_rts
	$(CP) -r ada-rts/*.gpr ada-rts/*.ads ada-rts/*.adb \
	   $(PREFIX)/share/$(DIRNAME)/gnatcov_ada_rts
	# Copy gnatcov_rts.ads from the regular runtime to the Ada only one to make
	# sure we keep the runtime versions in sync
	$(CP) rts/gnatcov_rts.ads $(PREFIX)/share/$(DIRNAME)/gnatcov_ada_rts
	# Create library/object directories for gnatcov_rts in the installed tree.
	# This is necessary for out-of-tree builds of gnatcov_rts to work, as
	# gnatcov_rts may be read-only after final installation, and gprbuild
	# expects these directories to exist even if it does not need to modify
	# them.
	for d in obj lib; do \
	  for lt in static static-pic relocatable; do \
	    mkdir $(PREFIX)/share/$(DIRNAME)/gnatcov_rts/$$d-$$lt; \
	  done; \
	done
	$(MKDIR) $(PREFIX)/share/$(DIRNAME)/gnatcov_ada_rts/obj-gnatcov_rts

install-lib:
	$(MKDIR) $(PREFIX)/lib/gnatcoverage
	$(CP) gnatcov-xml-report.xsd $(PREFIX)/lib/gnatcoverage
	$(CP) cobertura.dtd $(PREFIX)/lib/gnatcoverage

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
	sed -e "s/^version=.*/version=\"$$($(PGM64PATH) --version)\"/" \
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

# GNATsas

# This is equivalent to the default run performed during CI
GNATSAS_ARGS=-XBUILD_MODE=$(BUILD_MODE) -XPART=gnatcov64
GNATSAS_OBJ_DIR=obj64/$(BUILD_MODE)
gnatsas:
	rm -f $(OBJ_DIR)/results
	gnatsas analyze -P gnatcov.gpr $(GNATSAS_ARGS)
	gnatsas report text -P gnatcov.gpr $(GNATSAS_ARGS)
	if gnatsas report exit-code -P gnatcov.gpr $(GNATSAS_ARGS); then \
		echo "gnatsas:PASSED" >> $(GNATSAS_OBJ_DIR)/results ; \
	else \
		echo "gnatsas:FAILED:unexpected messages" >> $(GNATSAS_OBJ_DIR)/results ; \
		gnatsas report -P gnatcov.gpr -o $(GNATSAS_OBJ_DIR)/gnatsas.out \
      $(GNATSAS_ARGS) ; \
	fi
	# GNAT SAS report root should be the repository root so file URIs are
	# correctly resolved in GitLab. This is executed from tools/gnatcov in the
	# CI.
	gnatsas report code-climate -P gnatcov.gpr --root ../.. \
    -o $(GNATSAS_OBJ_DIR)/gnatsas/gnatsas.json $(GNATSAS_ARGS) ;

bump_baseline:
	gnatsas baseline --bump-baseline -P gnatcov.gpr $(GNATSAS_ARGS)

.PHONY: force

force:
