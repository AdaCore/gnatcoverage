"""
Check that "gnatcov instrument" does not remove instrumented sources in the
object directory of externally built projects.
"""

import os.path

from e3.fs import ls, rm

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

opslib_gpr = os.path.join("..", "opslib", "opslib.gpr")
tests_gpr = os.path.join("..", "tests.gpr")
tests_obj_dir = os.path.join("..", "obj")
sid_pattern = os.path.join("..", "opslib", "obj-opslib", "*.sid")

tests_gprsw = GPRswitches(
    root_project=tests_gpr,
    xvars=[("OPSLIB_EXTERNALLY_BUILT", "true")],
    externally_built_projects=True,
)

# First, instrument and build the opslib library project
xcov_instrument(
    gprsw=GPRswitches(root_project=opslib_gpr), covlevel="stmt+decision"
)
gprbuild(opslib_gpr, trace_mode="src", out="gprbuild-opslib.out")

# Now instrument, build and run the test driver. When run on this test project,
# the instrumenter used to remove the instrumented sources in the opslib
# directory, which is invalid since opslib is externally built during this
# step, and as a result, building the instrumented main (from tests.gpr) failed
# because of the missing buffer units in opslib.
build_run_and_coverage(
    gprsw=tests_gprsw,
    covlevel="stmt+decision",
    mains=["test_inc"],
    extra_coverage_args=["-axcov", "--output-dir=report"],
    gpr_obj_dir=tests_obj_dir,
    gpr_exe_dir=tests_obj_dir,
    trace_mode="src",
)

check_xcov_reports(
    "report",
    {"ops.ads.xcov": {}, "ops.adb.xcov": {"+": {4, 5}, "-": {6}}},
    discard_empty=False,
)

# Re-run the instrumenter on the main, to check that it does not re-create a
# SID file for the opslib externall built project.
rm(sid_pattern)
xcov_instrument(gprsw=tests_gprsw, covlevel="stmt+decision")
sids = ls(sid_pattern)
thistest.fail_if(
    sids,
    '"gnatcov instrument" wrongly re-created SID files: {}'.format(
        sorted(sids)
    ),
)

thistest.result()
