GPRBUILD=gprbuild
GPRCLEAN=gprclean
PREFIX=install
PARALLEL_BUILD=0

include ../../host.mk

.PHONY: force

# Files for libexec/gnatcoverage include binary executables
# (which need to be built first), and a python script.

# Despite being named Trace32, the probes support both 32 and 64 bit targets,
# so we need to build a trace adapter both for 32 bit and 64 bit targets.
EXENAMES=trace32_drv_32 trace32_drv_64

# Directory name in the install tree
DIRNAME=gnatcoverage

DOCFORMATS=pdf html

# By default, assume that the binutils dependency was built in the source tree.
BINUTILS_BUILD_DIR?=$(BINUTILS_SRC_DIR)

all: bin

# Here we build trace32_drv_32 and trace32_drv_64 executables. State
# we don't need source instrumentation support at all in these programs
# to minimize the executable sizes.

BIN_COMMON_BUILD_ARGS=\
 -Ptrace32.gpr -p -j$(PARALLEL_BUILD) \
 -XBINUTILS_SRC_DIR="$(BINUTILS_SRC_DIR)" \
 -XBINUTILS_BUILD_DIR="$(BINUTILS_BUILD_DIR)" \
 -largs $(LD_FLAGS) \
 -cargs:c++ $(CXXFLAGS) \
 $(EXTRA_ADAFLAGS) \
 -gargs

bin: force
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=lib64
	$(GPRBUILD) $(BIN_COMMON_BUILD_ARGS) -XPART=lib32

###################
# General cleanup #
###################

.PHONY: clean

clean:
	$(GPRCLEAN) -Ptrace32.gpr -XPART=lib64
	$(GPRCLEAN) -Ptrace32.gpr -XPART=lib32

################
# Installation #
################

.PHONY: install-libexec install

install-libexec:
	$(MKDIR) $(PREFIX)/libexec/$(DIRNAME)
	$(CP) $(addsuffix $(exeext),$(EXENAMES)) \
          $(PREFIX)/libexec/$(DIRNAME)

install: install-libexec
