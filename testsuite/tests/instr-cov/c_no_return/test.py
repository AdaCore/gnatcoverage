"""
Check that gnatcov always insert a call to dump buffers, even when the main
does not end with a return statement, when the dump-trigger is main-end.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp_no_return = Wdir("tmp_no_return")

# Check when the main function does not exit through a return
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[os.path.join("..", "no_return")], mains=["main.c"])
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    dump_trigger="main-end",

    # With older toolchains, a missing return results in an arbitrary exit
    # code, so let's ignore failures.

    register_failure=False,
)
check_xcov_reports(
    "*.xcov", {"main.c.xcov": {"+": {4, 5, 6, 12, 13}, "-": {14}}}, "xcov"
)

tmp_no_return.to_homedir()
tmp = Wdir("tmp_empty_main")

# Check that we have a source trace even if the main is empty
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[os.path.join("..", "empty_main")], mains=["main.c"]
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    dump_trigger="main-end",
)
thistest.result()
