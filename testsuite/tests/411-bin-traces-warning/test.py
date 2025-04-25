"""
Check that gnatcov emits the expected warning when binary traces are involved.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)


tmp = Wdir("tmp_")

warning = (
    "warning: Support for coverage of non-instrumented programs is deprecated"
    " and will disappear after GNATcoverage 26 releases. You are encouraged to"
    " migrate to instrumentation-based coverage: you can read more about it in"
    " our documentation: <https://docs.adacore.com/live/wave/gnatdas/html/"
    "gnatdas_ug/gnatcov/src_traces.html>"
)


def check_warning(output_file):
    """Check the presence of the expecetd warning in the given text file."""
    thistest.fail_if(
        warning not in lines_of(output_file),
        f"Missing expected warning in {output_file}",
    )


# Remove the warning-disabling env var that we set at the testsuite level,
# since our goal here is precisely to check that it is emitted.
os.environ.pop("GNATCOV_NO_BINARY_TRACES_WARNING")

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])
gprbuild(gpr)

# Check that we have the expecetd warning both in "gnatcov run" and "gnatcov
# coverage" for binary traces.
xrun(exepath_to("main"), out="run.txt")
check_warning("run.txt")

xcov(
    [
        "coverage",
        "-P",
        gpr,
        "-cstmt",
        "-axcov",
        "--output-dir=xcov-bin",
        tracename_for("main"),
    ],
    out="coverage-bin.txt",
)
check_warning("coverage-bin.txt")
check_xcov_reports("xcov-bin", {"main.adb.xcov": {"+": {5}}})

# Check that we do not have it in "gnatcov instrument" nor "gnatcov coverage"
# for source traces.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov-src"],
    trace_mode="src",
)
thistest.fail_if_not_equal(
    "output of 'gnatcov instrument' (instrument.log)",
    "",
    contents_of("instrument.log"),
)
thistest.fail_if_not_equal(
    "output of 'gnatcov coverage' for src traces (coverage.log)",
    "",
    contents_of("coverage.log"),
)
check_xcov_reports("xcov-src", {"main.adb.xcov": {"+": {5}}})

thistest.result()
