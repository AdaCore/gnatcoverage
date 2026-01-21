"""
Check that gnatcov does not reinstrument any unit when instrumenting
consecutively a project without any file modification.
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

# The first instrumentation command should instrument everything
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
)

# Then, the second instrumentation command should not instrument anything
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
