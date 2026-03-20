"""
Check that GNAT can compile instrumented code for overriding null procedures
that have a separate declaration.
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
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {6, 7, 9, 10}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {5, 6}},
    },
    discard_empty=False,
)

thistest.result()
