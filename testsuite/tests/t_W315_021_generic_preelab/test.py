"""
Check that declarations of a generic preelaborate Ada unit are not
instrumented, as it would otherwise result in uncompilable code.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["test.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=["--annotate=xcov"],
)

check_xcov_reports(
    "obj",
    {
        "test.adb.xcov": {"+": {4, 6}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {4}},
    },
    discard_empty=False,
)

thistest.result()
