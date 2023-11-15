"""
Check that gnatcov does not yield "unknown warning warnings" when parsing a
file with warnings not recognized by clang.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[".."], mains=["main.c"], main_cargs=["-Wtrampolines"]
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports("xcov/*.xcov", {"xcov/main.c.xcov": {"+": {4}}})

thistest.result()
