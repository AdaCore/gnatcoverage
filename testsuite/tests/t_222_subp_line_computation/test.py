"""
Test that line state computation is done for lines which contains both SCOs for
a subprogram of interest and SCOs outside of all subprograms of interest.

Gnatcov used to not compute the coverage state of a line if it contained a SCO
not in the subprogram of interest.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]), units=["pkg"]
    ),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["--subprograms=../pkg.ads:5", "-axcov+"],
)

check_xcov_reports("obj", {"pkg.ads.xcov": {"+": {5}}})

thistest.result()
