"""
Check that using --dump-trigger=manual works correctly when gnatcov is not run
in the same directory as where the project's output directory is created.

In these situations, gnatcov used to not emit the dump helper unit, as gnatcov
used to check the existence of the basename of the object directory, which was
thus not found.
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


# Create an object directory deeply nested, so that gnatcov is not run in the
# same directory as where it will be created.
obj_dir = "deep/obj/dir"

# We can't build_run_and_coverage as the default trace name only depends on the
# project name so all mains would dump a trace with the same name (execution
# last less than a second). Instead, instrument and execute the first main
# through build_and_run, then run the other mains manually.
cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid="p",
            srcdirs=[".."],
            objdir=obj_dir,
            mains=["main_ada.adb", "main_c.c", "main_cpp.cpp"],
            langs=["Ada", "C", "C++"],
        )
    ),
    covlevel="stmt",
    mains=["main_ada"],
    gpr_obj_dir=obj_dir,
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
    "*.xcov",
    {
        "main_ada.adb.xcov": {"+": {5}},
        "main_c.c.xcov": {"+": {6}, "-": {8}},
        "main_cpp.cpp.xcov": {"+": {6}, "-": {8}},
    },
    cwd=obj_dir,
)

thistest.result()
