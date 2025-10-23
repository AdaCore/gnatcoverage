"""
Test that the subprogram of interest feature works correctly for expression
functions which have a previous declaration.

gnatcov used to register the actual expression function's sloc as identifier
instead of it first declaration, resulting in an error message being emitted.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]), units=["Pkg"]
    ),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["--subprograms=../pkg.ads:2", "-axcov"],
)

check_xcov_reports("obj", {"pkg.ads.xcov": {}, "pkg.adb.xcov": {"+": {5}}})

thistest.result()
