# This support Makefile is a helper for examples packaged together with the
# xcov/qemu bundle for coverage analysis.  The intent is to have this Makefile
# invoked from the example dedicated directories.

# It is useful both to factorize the work for different examples and to
# illustrate a typical coverage data generation sequence for a program
# starting from sources.

# The basic idea is
#  1/ Build the program executable from the sources
#  2/ Run it through qemu to get execution traces
#  3/ Use xcov to produce user level coverage data from the traces

QEMU=qemu-system-ppc
XCOV=xcov
CFLAGS=-g -O1 -fpreserve-control-flow
AFLAGS=-gnaty

# Set of main target programs for which we want to produce coverage info,
# to be defined from the command line or by includers:
# TESTS=

# Both a target specific support subdir and a target toolchain prefix:
TARGET = powerpc-elf

# Access to common support files like this Makefile and to target specific
# sources such as the qemu bsp:
BASE_SUPPORT_DIR = ../support
TARGET_SUPPORT_DIR = $(BASE_SUPPORT_DIR)/$(TARGET)
SUPPORT_DIRS=$(BASE_SUPPORT_DIR) $(TARGET_SUPPORT_DIR)

RM=rm

.PRECIOUS: %.trace

all: build trace analyze

# XCOV : produce annotated sources or html from a trace file and the
# corresponding executable

analyze: $(TESTS:%=index-%.html)

index-%.html: %.trace
	$(XCOV) -r $< -e $(basename $<) \
	 --output-format=html --asm --source-coverage
	mv index.html $@

# QEMU : produce execution trace from executable program execution within
# instrumented target emulation environment.

trace: $(TESTS:%=%.trace)

%.trace: % force
	xcov --run $< --target=$(TARGET)

# CROSS toolchain : produce executable program from sources + target bsp

CROSS = $(TARGET)-

build: $(TESTS:%=%.bin)

%.bin: %
	$(CROSS)objcopy -O binary $< $@

$(TESTS): start.o c_io.o force
	$(CROSS)gnatmake $(CFLAGS) $(AFLAGS) $@ $(SUPPORT_DIRS:%=-I%) \
	-largs start.o c_io.o -Wl,-T$(TARGET_SUPPORT_DIR)/gnu.ld -nostdlib

start.o: $(TARGET_SUPPORT_DIR)/start.s
	$(CROSS)gcc -c -o $@ $<

c_io.o: $(TARGET_SUPPORT_DIR)/c_io.c
	$(CROSS)gcc -c $(CFLAGS) -o $@ $<

# Housekeeping

clean:
	$(RM) -f *.o *.ali *.bin *.trace b~* *~ *.xcov $(TESTS)

.PHONY: force clean
