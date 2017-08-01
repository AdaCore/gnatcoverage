GPRBUILD=gprbuild
GPRCLEAN=gprclean
PREFIX=install

include ../../host.mk

.PHONY: force

ARCH=32

# Files for libexec/gnatcoverage include binary executables
# (which need to be built first), and a python script.

EXENAMES=trace32_drv

# Directory name in the install tree
DIRNAME=gnatcoverage

DOCFORMATS=pdf html

# By default, assume that the binutils dependency was built in the source tree.
BINUTILS_BUILD_DIR?=$(BINUTILS_SRC_DIR)

all: $(EXENAMES)

$(EXENAMES): force
	$(GPRBUILD) -Ptrace32 -XARCH=$(ARCH) $(EXTRA_ADAFLAGS) $@ \
	-XBINUTILS_SRC_DIR="$(BINUTILS_SRC_DIR)" \
	-XBINUTILS_BUILD_DIR="$(BINUTILS_BUILD_DIR)" \


###################
# General cleanup #
###################

.PHONY: clean

clean:
	$(GPRCLEAN) -Ptrace32

################
# Installation #
################

.PHONY: install-libexec install

install-libexec:
	$(MKDIR) $(PREFIX)/libexec/$(DIRNAME)
	$(CP) $(addsuffix $(exeext),$(EXENAMES)) \
          $(PREFIX)/libexec/$(DIRNAME)

install: install-libexec
