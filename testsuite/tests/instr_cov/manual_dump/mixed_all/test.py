"""
Check that using --dump-trigger=manual works correctly in projects where a
manual dump indication is present in an Ada source and in all supported C-like
sources.

This is a regression test, gnatcov used to emit helper units with colliding
object filenames, resulting in the instrumented sources failing to build.
"""

import os

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, run_cov_program, exepath_to

Wdir("tmp_")

# Name for the main_c and main_cpp executable traces
c_trace_name = "main_c.srctrace"
cpp_trace_name = "main_cpp.srctrace"

# We can't build_run_and_coverage as the default trace name only depends on the
# project name so all mains would dump a trace with the same name (execution
# last less than a second). Instead, instrument and execute the first main
# through build_and_run, then run the other mains manually.
cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid="p",
            srcdirs=[".."],
            mains=["main_ada.adb", "main_c.c", "main_cpp.cpp"],
            langs=["Ada", "C", "C++"],
        )
    ),
    covlevel="stmt",
    mains=["main_ada"],
    dump_trigger="manual",
    manual_prj_name="p",
    extra_coverage_args=["-axcov", c_trace_name, cpp_trace_name],
)

env = dict(os.environ)
env["GNATCOV_TRACE_FILE"] = c_trace_name
run_cov_program(exepath_to("main_c"), env=env)

env["GNATCOV_TRACE_FILE"] = cpp_trace_name
run_cov_program(exepath_to("main_cpp"), env=env)

xcov(cov_args)
check_xcov_reports(
    "obj",
    {
        "main_ada.adb.xcov": {"+": {5}},
        "main_c.c.xcov": {"+": {6}, "-": {8}},
        "main_cpp.cpp.xcov": {"+": {6}, "-": {8}},
    },
)

thistest.result()
