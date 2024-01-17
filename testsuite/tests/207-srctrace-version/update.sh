#!/bin/bash

gnatcov instrument -P tmp_/gen.gpr -cstmt+mcdc
cp tmp_/obj/*.sid .
gprbuild -P tmp_/gen.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr
GNATCOV_TRACE_FILE=reference.srctrace ./tmp_/main
