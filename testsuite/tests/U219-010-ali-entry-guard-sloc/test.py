"""
Regression test for source location of dominance markers referencing
decisions in entry guards. It used to be wrong in ali files.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports, checked_xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

# Check that there is no warning

checked_xcov(xcov_args, "coverage.log")

check_xcov_reports("obj/*.xcov", {
    "obj/main.adb.xcov": {"+": {11, 12, 13}, "-": {16, 17, 20, 21}},
})

thistest.result()
