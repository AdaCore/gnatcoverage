"""
Check that gnatcov correctly processes compilation units when the root node is
a protected body. It used to not consider it as a scope, which resulted in
declarations scope not having a parent scope, making `gnatcov coverage` crash.
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
        "main.adb.xcov": {"+": {4, 6, 7, 8}},
        "a.ads.xcov": {"+": {3}},
        "a.adb.xcov": {},
        "a-b.adb.xcov": {"+": {8, 10, 13, 20, 26}, "-": {11}},
    },
    discard_empty=False,
)

thistest.result()
