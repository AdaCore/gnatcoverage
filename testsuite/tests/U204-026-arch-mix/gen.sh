#! /bin/sh

# Script to generate instrumented sources, SID, traces and checkpoint files to
# be used in this test.
#
# Since our testsuite infrastructure does not allow us to run several hosts and
# toolchains at the same time, we pre-generate these files manually and add
# them as material to the testsuite.

set -ex

os="$1"
case "$os" in
    linux)
        expected_cwd="/tmp/U204-026-arch-mix"
        ;;
    windows)
        expected_cwd="/cygdrive/c/tmp/U204-026-arch-mix"
        ;;
    *)
        echo "Invalid OS: $os"
        exit 1
        ;;
esac
if [ "$expected_cwd" != "$PWD" ]
then
    echo "Please run this script from the $expected_cwd directory"
    exit 1
fi

XCOV_INSTR="gnatcov instrument -Pfoo -cstmt+mcdc \
    --dump-trigger=main-end --dump-channel=base64-stdout"
XCOV_COV="gnatcov coverage -Pfoo -cstmt+mcdc"
CROSS32_OPTS="--target=arm-eabi --RTS=light-stm32f4"
CROSS64_OPTS="--target=aarch64-elf --RTS=light-zynqmp"
SRC_BUILD_OPTS="--src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts"
SRC_BUILD_CROSS_OPTS="--src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts"
BIN_BUILD_OPTS="-cargs -g -fdump-scos -fpreserve-control-flow"

rm -rf gen
mkdir gen

#
# 1. Check that we can instrument on one host and build+run+coverage on another
#

# On Windows, instrument the "foo" project for the native target.  "test.py"
# will find the SIDs and the instrumented sources in "gen/x86_64-windows".
#
# On Linux, instrument the "foo" project for ARM ELF. "test.py" will find the
# SIDs and the instrumented sources in "gen/arm-elf-linux"

case $os in
    windows)
        $XCOV_INSTR
        dest=x86_64-windows
        ;;
    linux)
        $XCOV_INSTR $CROSS32_OPTS
        dest=arm-elf-linux
        ;;
esac
mkdir gen/$dest
cp obj/*.sid obj/foo-gnatcov-instr/* gen/$dest

#
# 2. Check that we can mix checkpoints created from source traces even when
#    these source traces were produced for different targets, possibly from
#    different hosts.
#

# On Windows, build the "foo" instrumented project (previously for the native
# target) and run main_1 to produce a source trace.
#
# On Linux, do the same for ARM ELF/main_2.
#
# In both cases, create a checkpoint from the trace.

case $os in
    windows)
        gprbuild -Pfoo $SRC_BUILD_OPTS
        obj/main_1 > main_1-out.txt
        gnatcov extract-base64-trace main_1-out.txt gen/main_1.srctrace
        $XCOV_COV --save-checkpoint=gen/src-main_1.ckpt gen/main_1.srctrace
        ;;
    linux)
        gprbuild $CROSS32_OPTS -Pfoo $SRC_BUILD_CROSS_OPTS
        arm-eabi-gnatemu --board=stm32f4 obj/main_2 > main_2-out.txt
        gnatcov extract-base64-trace main_2-out.txt gen/main_2.srctrace
        $XCOV_COV --save-checkpoint=gen/src-main_2.ckpt gen/main_2.srctrace
        ;;
esac

#
# 3. Reject mix of checkpoints created with binary traces for different
#    "bits-target" (32 vs 64-bit).
#

# On Linux, build+run+create checkpoints (with binary traces) for both 32 and
# 64-bit targets.

case $os in
    linux)
        gprbuild -Pfoo $CROSS32_OPTS $BIN_BUILD_OPTS
        gnatcov run --target=arm-eabi,stm32f4 -o bin-main_1.trace obj/main_1
        $XCOV_COV $CROSS32_OPTS --save-checkpoint=gen/bin-main_1.ckpt \
            bin-main_1.trace

        gprbuild -Pfoo $CROSS64_OPTS $BIN_BUILD_OPTS
        gnatcov run --target=aarch64-elf -o bin-main_2.trace obj/main_2
        $XCOV_COV $CROSS64_OPTS --save-checkpoint=gen/bin-main_2.ckpt \
            bin-main_2.trace
        ;;
esac

#
# 4. Check that we can use source traces produced on a different host
#

# Nothing to do here: step 2 already added source traces to the "gen"
# directory.

# Clean up permissions (i.e. remove execution bits on Windows)
find gen -type f | xargs chmod 644
