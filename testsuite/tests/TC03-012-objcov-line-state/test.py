"""
Testcase for TC03-012: check that gnatcov reports as uncovered an instruction
that is not executed, but that follows an executed instruction and has the same
sloc.
"""

from e3.fs import cp, mkdir

from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    xcov,
    xrun,
    tracename_for,
)

Wdir("tmp_")

# Compile all C sources
original_prj = gprfor(
    srcdirs=[".."],
    mains=["main.c"],
    objdir="obj-original",
    prjid="original",
    langs=["C"],
)
gprbuild(original_prj, extracargs=["-save-temps"])

# Use our custom assembly source for fact.c
mkdir("src")
cp("../main.c", "src/main.c")
cp("../fact.s", "src/fact.s")
prj = gprfor(srcdirs=["src"], mains=["main.c"], prjid="gen")
gprbuild(prj)

# Run and compute the XCOV coverage report
xrun(["-cinsn", exepath_to("main")])
xcov(["coverage", "-cinsn", "-axcov", "-Pgen", tracename_for("main")])

# Check that line 4 is correctly marked as partially covered, as it contains
# an uncovered instruction that follows an (always taken) branch (see the
# comment in fact.s).
check_xcov_reports(
    "obj",
    {
        "fact.c.xcov": {"+": {6, 7, 9}, "-": {5}, "!": {4}},
        "main.c.xcov": {"+": {6}},
    },
)

thistest.result()
