include ../../architecture.mk
include ../../host.mk

#########
# Build #
#########

VALGRIND_INCLUDE=$(VALGRIND_PREFIX)/include/valgrind

# Figure out the place from which we need to fetch the valgrind
# libraries to craft our coverage tool. We rely on this expanding into
# a single dir. We use of a wildcard expansion to find either
# prefix/lib or prefix/lib64, then need to watch out for a possible
# prefix/libexec.

VALGRIND_LIBDIRS=$(wildcard $(VALGRIND_PREFIX)/lib*/valgrind)
VALGRIND_LIBS=$(filter-out %/libexec/valgrind, $(VALGRIND_LIBDIRS))

CPP_FLAGS=-I. -I$(VALGRIND_INCLUDE) \
          -DVGA_$(VALGRIND_ARCH)=1 -DVGO_linux=1 \
          -DVGP_$(VALGRIND_ARCH)_linux=1 \
          -DVGPV_$(VALGRIND_ARCH)_linux_vanilla=1

# TODO:where did the -mpreferred-stack-boundary=2 option come from?

# Some versions of valgrind (observed with 3.16) fail loading the tool
# when it has a .note.gnu.property section, with an error like
#
#   valgrind: mmap(0x400000, 12288) failed in UME with error 22
#
# -Wa,-mx86-used-note=no is intended to prevent the production of
# such .note sections.

CFLAGS=-O2 -g \
       -fno-strict-aliasing -fno-builtin  -fno-stack-protector \
       -Wa,-mx86-used-note=no \
       -Wall -Wmissing-prototypes -Wshadow -Wpointer-arith -Wstrict-prototypes \
       -Wmissing-declarations -Wno-format-zero-length \
       -Wno-long-long  -Wno-pointer-sign

LDFLAGS=-static -nodefaultlibs -nostartfiles -u _start -Wl,--build-id=none

LIBS=$(VALGRIND_LIBS)/libcoregrind-$(VALGRIND_ARCH)-linux.a \
     $(VALGRIND_LIBS)/libvex-$(VALGRIND_ARCH)-linux.a -lgcc

COVERAGE_TOOL=coverage-$(VALGRIND_ARCH)-linux

all: $(COVERAGE_TOOL)

%.o: %.c
	gcc $(CPP_FLAGS) $(CFLAGS) -c $< -o $@

COVERAGE_OBJECTS=cov_traces.o cov_main.o

cov_traces.o: cov_traces.c cov_traces.h
cov_main.o: cov_main.c cov_traces.h

# The address 0x38000000 is the valt_load_address value from valgrind build.
# This value should be changed for Darwin (0x138000000).
$(COVERAGE_TOOL): $(COVERAGE_OBJECTS)
	gcc $(CFLAGS) -o $@ $(COVERAGE_OBJECTS) \
            $(LDFLAGS) -Wl,-Ttext=0x38000000 $(LIBS)

################
# Installation #
################

.PHONY: install

install:
	$(MKDIR) $(VALGRIND_TOOL_INSTALL_DIR)
	cp -p $(COVERAGE_TOOL) $(VALGRIND_TOOL_INSTALL_DIR)

###########
# Cleanup #
###########

.PHONY: clean

clean:
	rm -f $(COVERAGE_TOOL) $(COVERAGE_OBJECTS)
