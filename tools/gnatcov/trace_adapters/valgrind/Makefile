
MKDIR=mkdir -p

VALGRIND_DIR=$(shell dirname `which valgrind`)
VALGRIND_INCLUDE=$(VALGRIND_DIR)/../include/valgrind
VALGRIND_LIBS=$(VALGRIND_DIR)/../lib/valgrind

CPP_FLAGS=-I. -I$(VALGRIND_INCLUDE) \
          -DVGA_x86=1 -DVGO_linux=1 -DVGP_x86_linux=1 -DVGPV_x86_linux_vanilla=1

CFLAGS=-mpreferred-stack-boundary=2 -O2 -g \
       -fno-strict-aliasing -fno-builtin  -fno-stack-protector \
       -Wall -Wmissing-prototypes -Wshadow -Wpointer-arith -Wstrict-prototypes \
       -Wmissing-declarations -Wno-format-zero-length \
       -Wno-long-long  -Wno-pointer-sign

LDFLAGS=-static -nodefaultlibs -nostartfiles -u _start -Wl,--build-id=none

LIBS=$(VALGRIND_LIBS)/libcoregrind-x86-linux.a \
     $(VALGRIND_LIBS)/libvex-x86-linux.a -lgcc

COVERAGE_TOOL=coverage-x86-linux

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

# Install within a provided VALGRIND_PREFIX if one is provided, within PREFIX
# otherwise. In both cases, take this as the root of a place where a regular
# valgrind install is expected to be.

ifeq (,$(VALGRIND_PREFIX))
VALGRIND_PREFIX=$(PREFIX)
endif

TOOL_INSTALL_DIR=$(VALGRIND_PREFIX)/lib/valgrind

.PHONY: install check-prefix

check-prefix:
	@if [ ! -e $(VALGRIND_PREFIX)/bin/valgrind ]; then \
	  echo "warning: valgrind missing from prefix where adapter is installed."; \
	fi

install: check-prefix
	$(MKDIR) $(TOOL_INSTALL_DIR)
	cp -p $(COVERAGE_TOOL) $(TOOL_INSTALL_DIR)

###########
# Cleanup #
###########

.PHONY: clean

clean:
	rm -f $(COVERAGE_TOOL) $(COVERAGE_OBJECTS)
