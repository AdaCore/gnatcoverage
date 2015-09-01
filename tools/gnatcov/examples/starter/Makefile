# Invoke this with the TARGET and RTS you wish to exercise.
# For example 
#   make TARGET=powerpc-elf RTS=zfp-prep
# or
#   make TARGET=leon-elf RTS=ravenscar
#
# Leaving TARGET undefined conveys intent to exercise for
# the current native configuration.

ANNOTATE=html

TARGET=
ifneq ($(strip $(TARGET)),)
TARGET_ARG=--target=$(TARGET)
endif

RTS=
ifneq ($(strip $(RTS)),)
RTS_ARG=--RTS=$(RTS)
endif

all:
	gprbuild -p -f $(TARGET_ARG) $(RTS_ARG) -Pops.gpr test_inc.adb \
	 -cargs -g -fpreserve-control-flow -fdump-scos
	gnatcov run $(TARGET_ARG) obj/test_inc
	gnatcov coverage --level=stmt --annotate=$(ANNOTATE) test_inc.trace \
          $(TARGET_ARG) -Pops.gpr
