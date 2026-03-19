"""
Check that the instrumentation of the s-tpopsp.adb and s-tporft.adb sources in
the Ada runtime completes (it used to crash).
"""

import os.path

from e3.fs import sync_tree

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Create a copy of the runtime to avoid polluting the test directory
sync_tree("../runtime", "runtime")

runtime_prj = os.path.abspath("runtime/ravenscar_build.gpr")
runtime_dir = os.path.dirname(runtime_prj)

xcov_instrument(
    gprsw=GPRswitches(root_project=runtime_prj),
    covlevel="stmt+mcdc",
    auto_config_args=False,
    auto_target_args=False,
    extra_args=["--target", thistest.env.target.triplet, "--RTS", runtime_dir],
    # For practical purposes, this test instruments a fake runtime. Since this
    # runtime is incomplete, it is not possible to build the coverage runtime
    # for it: we cannot run "gnatcov setup" and thus we expect a warning about
    # the runtime mismatch.
    tolerate_messages="Current runtime is",
)

thistest.result()
