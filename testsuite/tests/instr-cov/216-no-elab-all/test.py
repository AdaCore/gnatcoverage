"""
Check that units with the No_Elaboration_Code_All pragma still compile after
instrumentation.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {5},
            "!": {7, 9},
            "-": {8, 10},
        },
        "pkg.adb.xcov": {"!": {9}},
        "pkg.ads.xcov": {},
    },
    discard_empty=False,
)

thistest.result()
