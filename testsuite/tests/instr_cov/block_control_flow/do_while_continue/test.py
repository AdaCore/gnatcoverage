"""
Regression test: check that block instrumentation ends the current block before
the condition of a do-while statement.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_instr_args=["--instrument-block"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.c.xcov": {"+": {6, 7}},
        "pkg.c.xcov": {"+": {6, 7, 10, 11, 14, 15}, "-": {12}},
    },
    discard_empty=False,
)

thistest.result()
