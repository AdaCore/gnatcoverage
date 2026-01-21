"""
Check that gnatcov reinstruments a unit when instrumenting every part of the
unit after it has been partially instrumented (by using
--excluded-source-files).
"""

from SCOV.minicheck import (
    build_run_and_coverage,
    check_xcov_reports,
    xcov_instrument,
)
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

root_prj = gprfor(srcdirs=[".."], mains=["main.c"])

# The first instrumentation command instruments skips the instrumentation of
# the header file.
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    extra_args=["--excluded-source-files=pkg.h"],
)

# Then, instrument the header file and check that gnatcov reinstruments pkg.c.
# Also check coverage results for additional safety.
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.log",
)

check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {"+": {8, 9}},
        "pkg.c.xcov": {"+": {6}},
        "pkg.h.xcov": {"+": {5}},
    },
)
thistest.result()
