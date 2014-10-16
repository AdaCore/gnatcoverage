# Invoke this with the TARGET and RTS you wish to exercise.
# For example 
#   make TARGET=powerpc-elf RTS=zfp-prep
# or
#   make TARGET=leon-elf RTS=ravenscar

ANNOTATE=html

TARGET=TARGET_WAS_NOT_SET
RTS=RTS_WAS_NOT_SET

all:
	gprbuild -p -f --target=$(TARGET) --RTS=$(RTS) -Pops.gpr test_inc.adb \
	 -cargs -g -fpreserve-control-flow -fdump-scos
	gnatcov run --target=$(TARGET) obj/test_inc
	gnatcov coverage --level=stmt --annotate=$(ANNOTATE) test_inc.trace \
          --target=$(TARGET) -Pops.gpr
