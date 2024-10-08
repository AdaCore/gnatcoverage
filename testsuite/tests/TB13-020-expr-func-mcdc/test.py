"""
Regression testcase for the instrumentation of expression functions that
trigger MC/DC obligations.

(No reason not to also run it with binary traces.)
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

p = gprfor(prjid="p", srcdirs=[".."], mains=["main.adb"])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=report"],
)

check_xcov_reports(
    "report",
    {
        "main.adb.xcov": {"!": {6, 10, 16, 20}, "+": {12, 13}, "-": {17, 21}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {9}},
    },
    discard_empty=False,
)

thistest.result()
