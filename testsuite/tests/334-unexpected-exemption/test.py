"""
Check that --annotate=xcov,report produces expected '*' lines for
exempted-and-uncovered code. It used to generate '#' lines
(exempte-and-covered) instead.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov,report", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5}},
        "proc.adb.xcov": {
            "+": {5, 7},
            "-": {6},
            "#": {12, 13, 14},
            "*": {8, 9, 10},
        },
    },
)

thistest.result()
