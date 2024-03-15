"""
Check that the instrumentation of a project does not crash when the Ada runtime
for this project contains homonym sources in its source directories.
"""

import os.path

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

# Avoid "creating output path" info messages
os.mkdir("obj")

xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+mcdc",
    auto_config_args=False,
    auto_target_args=False,
    extra_args=[
        "--target",
        thistest.env.target.triplet,
        "--RTS",
        os.path.abspath("../runtime"),
    ],
    # This test instruments a fake runtime. For practical purposes, that
    # runtime is incomplete: it is not possible to build the coverage runtime
    # for it, we cannot run "gnatcov setup" and thus we expect a warning about
    # the runtime mismatch.
    tolerate_messages="Current runtime is",
    out="instrument.log",
)

thistest.result()
