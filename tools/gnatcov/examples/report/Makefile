# This is a example of code with conditions/decisions. This Makefile
# lets you select target to use:

# make TARGET=native XCOVTARGET=i386-linux runs on x86-linux (native)
# make TARGET=powerpc-elf [BOARD=qemu-prep] runs on bare PowerPC
# make TARGET=leon-elf    runs on bare LEON2

TESTS=test_engines
XCOVLEVEL=stmt+mcdc
include ../support/Makefile.examples

analyze: $(TESTS:%=report-%.txt)
