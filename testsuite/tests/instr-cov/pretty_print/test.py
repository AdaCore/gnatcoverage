"""
Test that running gnatcov instrument with the --pretty-print switch does not
yield any warning.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs="..", mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--pretty-print"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5, 7, 8}},
        "pkg.adb.xcov": {"+": {5, 6, 8}},
        "pkg.ads.xcov": {},
    },
    discard_empty=False,
)

thistest.result()
