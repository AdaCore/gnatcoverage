"""
Check that using --dump-trigger=manual works correctly in projects where a
manual dump indication is present both in an Ada source and in a C-like source.

This is a regression test, gnatcov used to not emit the helper unit for one of
the language kinds, resulting in the instrumented sources failing to build.
"""

import os

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, run_cov_program, exepath_to

Wdir("tmp_")

# Name for the main_c executable trace
c_trace_name = "main_c.srctrace"

# We can't build_run_and_coverage as the default trace name only depends on the
# project name so both mains would dump a trace with the same name (execution
# last less than a second). Instead, instrument and execute the first main
# through build_and_run, then run the second main manually.
cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid="p",
            srcdirs=[".."],
            mains=["main_ada.adb", "main_c.c"],
            langs=["Ada", "C"],
        )
    ),
    covlevel="stmt",
    mains=["main_ada"],
    dump_trigger="manual",
    manual_prj_name="p",
    extra_coverage_args=["-axcov", c_trace_name],
)

env = dict(os.environ)
env["GNATCOV_TRACE_FILE"] = c_trace_name
run_cov_program(exepath_to("main_c"), env=env)
xcov(cov_args)
check_xcov_reports(
    "*.xcov",
    {
        "main_ada.adb.xcov": {"+": {5}},
        "main_c.c.xcov": {"+": {6}, '-': {8}},
    },
    cwd="obj")

thistest.result()
