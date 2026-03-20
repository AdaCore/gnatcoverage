"""
Regression test: GNATcoverage used to create an overriding expression function
when instrumenting an overriding expression function in a package body. This
overriding was declared too late, which resulted in a compilation error.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+uc_mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {6, 7, 9}},
        "pkg.ads.xcov": {"+": {5, 8, 9, 10}, "-": {6}},
        "pkg.adb.xcov": {"!": {6}},
    },
)

thistest.result()
