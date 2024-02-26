"""
Check that we can compute object coverage .xcov reports from
a binary which contains asm-level debug line info not attached
to any subprogram from the dwarf standpoint.
"""

import os.path

from e3.fs import rm

from SUITE.cutils import Wdir
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    thistest,
    tracename_for,
    xcov,
    xrun,
)

rm("tmp_", recursive=True)
tmp_ = Wdir("tmp_")

# We are producing object coverage reports and need fine control over the
# compilation options here
_cargs = {"scovcargs": False, "suitecargs": False}

# First stage: compile the C source code to assembly. We want no debug
# information at this stage.
gpr_stage1 = gprfor(
    "foo.c",
    prjid="stage1",
    srcdirs="..",
    exedir=".",
    main_cargs="-g0 -save-temps",
    langs=("C",),
)
gprbuild(gpr_stage1, gargs="-c", **_cargs)

# Second stage: assemble to object code with debug information relative to the
# assembly.
gpr_stage2 = gprfor(
    "foo.s",
    prjid="stage2",
    srcdirs="obj",
    exedir=".",
    main_cargs="-g",
    langs=("Asm",),
)
gprbuild(gpr_stage2, **_cargs)

gnatcov_flags = ["-P", gpr_stage2, "--level=branch"]

# Run and compute code coverage.
xrun(gnatcov_flags + [exepath_to("foo")])
xcov(["coverage"] + gnatcov_flags + ["--annotate=xcov", tracename_for("foo")])

thistest.fail_if(
    not os.path.exists(os.path.join("obj", "foo.s.xcov")),
    "There is no XCOV report for foo.s",
)

thistest.result()
