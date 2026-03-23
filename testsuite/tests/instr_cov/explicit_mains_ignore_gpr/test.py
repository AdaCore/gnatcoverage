"""
Check that when mains are provided in the GPR file and in "gnatcov
instrument"'s command line only those in the command line are instrumented as
mains.
"""

import glob

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, srctracename_for
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

gpr_file = gprfor(mains=["foo.adb", "bar.adb"], srcdirs=[".."])

gprsw = GPRswitches(gpr_file)

build_and_run(
    gprsw=gprsw,
    covlevel="stmt",
    extra_coverage_args=[],
    trace_mode="src",
    # Only instrument foo.adb as a main
    extra_instr_args=["foo.adb"],
    # Build and execute foo.adb and bar.adb
    extra_gprbuild_args=["foo.adb", "bar.adb"],
    mains=["foo", "bar"],
    # We expect there not to be a trace file for bar, so don't register not
    # finding it as an error.
    register_failure=False,
)

# Check that we only get traces for foo
expected_traces = [srctracename_for("foo")]
actual_traces = sorted(glob.glob("*.srctrace"))

thistest.fail_if_not_equal(
    "Source trace files", expected_traces, actual_traces
)

thistest.result()
