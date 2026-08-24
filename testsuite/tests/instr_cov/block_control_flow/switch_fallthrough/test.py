"""
Regression test: check that block instrumentation ends the current block on a
case label.

A case label is an entry point in the control flow: entering the switch there
skips the statements of the previous alternative when it falls through. Those
statements must not share a block with the ones that follow the label, or the
witness of that block reports them as covered.
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
        "pkg.c.xcov": {"+": {6, 7, 13, 14, 18}, "-": {10, 11, 16}},
    },
    discard_empty=False,
)

thistest.result()
