"""
Check that, in Ada 2022 mode, expression functions that contain
'(Unrestricted_|Unchecked_)Access attribute references are correctly
instrumented. We used to introduce declare expressions in this case, which is
illegal.
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
    "*.xcov",
    {
        "main.adb.xcov": {
            "+": {7, 9, 10, 13, 16, 19, 22, 23, 25, 26, 28, 29, 31, 32}
        }
    },
    cwd="xcov",
)

thistest.result()
