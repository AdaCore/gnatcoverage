"""
Regression test: check that block instrumentation ends the current block at
the end of an extended return statement.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_instr_args=["--instrument-block"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {4, 7}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {6, 8, 9}, "-": {11}},
    },
    discard_empty=False,
)

thistest.result()
