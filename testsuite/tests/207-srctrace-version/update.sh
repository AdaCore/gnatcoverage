#! /bin/sh

# Script to generate SID and traces.

set -ex

expected_cwd="/tmp/207-srctrace-version"
if [ "$expected_cwd" != "$PWD" ]
then
    echo "Please run this script from the $expected_cwd directory"
    exit 1
fi

gnatcov instrument -P p.gpr -cstmt+mcdc
cp obj/*.sid .
gprbuild -P p.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr
GNATCOV_TRACE_FILE=reference.srctrace ./obj/main
