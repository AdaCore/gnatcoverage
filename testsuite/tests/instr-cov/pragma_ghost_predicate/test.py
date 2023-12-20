"""
Check that decisions in Ghost_Predicate aspects are not instrumented.

They used to be instrumented as decisions, and thus be reported as uncovered in
this example program.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.control import env
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "*.xcov",
    {
        "main.adb.xcov": {"+": {5}},
        "pkg.ads.xcov": {"+": {5, 10, 13}},
    },
    "xcov",
)

thistest.result()
