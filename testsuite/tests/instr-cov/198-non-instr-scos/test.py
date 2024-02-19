"""
Regression testcase specific to the Ada instrumenter: check that a
non-instrumented SCO in one source file (pkg.ads) does not trigger a crash in
the next source file that is instrumented for the same unit (pkg.adb).
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Avoid "creating output path" info messages
os.mkdir("obj")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    tolerate_instrument_messages=(
        ".*cannot instrument an expression function which.*"
    ),
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {4, 6}},
        "pkg.ads.xcov": {"?": {8}},
        "pkg.adb.xcov": {"-": {11}},
    },
)

thistest.result()
