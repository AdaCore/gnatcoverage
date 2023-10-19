"""
Check that we consider unit-specific compiler switches (that have an effect on
the preprocessing) when using the manual dump-trigger. We used to ignore them.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

prj = gprfor(
    prjid="main",
    srcdirs=[".."],
    mains=["main.c"],
    main_cargs=["-DMAIN=1"],
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--dump-trigger=manual"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "*.xcov",
    {
        "main.c.xcov": {"+": {5}, "-": {7}},
    },
    "xcov",
)

thistest.result()
