"""
Regression testcase: for a C/C++ source that contains a manual dump directive
followed by a reset directive, "gnatcov instrument" used to get stuck in an
infinite loop.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")
build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(srcdirs=[".."], mains=["main.c"]), units=["main.c"]
    ),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="gen",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports("xcov", {"main.c.xcov": {"+": {6}, "-": {12, 14}}})
thistest.result()
