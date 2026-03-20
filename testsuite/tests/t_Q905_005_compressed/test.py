"""
Test that gnatcov properly reads compressed ELF sections
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
main = exepath_to("foo")
trace = tracename_for("foo")

gpr = gprfor(["foo.adb"], srcdirs="..")
gprbuild(gpr, largs=["-Wl,--compress-debug-sections=zlib"])

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
check_xcov_reports(".", {"foo.adb.xcov": {"+": {6, 7, 9, 13}}})

thistest.result()
