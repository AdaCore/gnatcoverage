# This example features both an interactive program and a set of AUnit based
# tests for some of the program units. This Makefile lets you select the
# variant to exercise:

# make TESTS=explore            runs the interactive program
# make UNIT_TESTS=test_explore  runs the AUnit based tests

# Additionally, this Makefile lets you select target to use:

# make TARGET=i386-linux ...  runs on linux
# make TARGET=powerpc-elf ... runs on bare board PowerPC
# make TARGET=leon-elf ...    not supported (no text input)

ifeq ($(TARGET),leon-elf)
all:
	@echo Example not supported: LEON target does not provide text input
else
include ../support/Makefile.examples
endif