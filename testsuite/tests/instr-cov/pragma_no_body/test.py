"""
Check that units only containing "pragma No_Body;" are properly handled. They
used to make the instrumenter crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


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
        "main.adb.xcov": {"+": {5}},
        "orig_pkg.ads.xcov": {},
        "orig_pkg.adb.xcov": {"+": {4}},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
