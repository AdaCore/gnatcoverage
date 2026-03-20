"""
Check that "gnatcov coverage" does not crash when processing a binary with
debug info that contains multiple definitions for the same function over
different compilation units.

This testcase does this with the original customer use case: a C++ inline
function defined in a header included in several compilation units.
"""

from SCOV.minicheck import checked_xcov, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.tutils import (
    exepath_to,
    gprfor,
    gprbuild,
    thistest,
    tracename_for,
    xrun,
)


wd = Wdir("tmp_")
main = exepath_to("main")
trace = tracename_for("main")

gpr = gprfor(["main.cpp"], srcdirs="..", langs=["C", "C++"])
gprbuild(gpr)

xrun([main], "run.log")
checked_xcov(
    [
        "coverage",
        "-P{}".format(gpr),
        "-cstmt",
        "-axcov",
        "--output-dir=.",
        trace,
    ],
    "coverage.log",
)
check_xcov_reports(".", {"bar.c.xcov": {"+": {4}}})

thistest.result()
