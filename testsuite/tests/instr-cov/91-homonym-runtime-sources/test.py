"""
Check that the instrumentation of a project does not crash when the Ada runtime
for this project contains homonym sources in its source directories.
"""

import os.path

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
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
    out="instrument.log",
)

thistest.fail_if_not_equal(
    "'gnatcov instrument' output",
    "",
    contents_of("instrument.log"),
)

thistest.result()
