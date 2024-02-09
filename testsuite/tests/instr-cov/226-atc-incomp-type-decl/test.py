"""
Check that "gnatcov instrument" does not crash when instrumenting an incomplete
type declaration for ATC(C).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+atc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "*.xcov",
    {
        "main.adb.xcov": {"+": {4, 6}},
        "pkg.ads.xcov": {},
    },
    cwd="xcov",
)

thistest.result()
