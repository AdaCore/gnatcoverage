"""
Check that when running gnatcov instrument two times, gnatcov does not
reinstrument any file.
"""

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Create a project
root_prj = gprfor(srcdirs=[".."], mains=["main.adb"])

# The first instrumentation command instruments all the files
xcov_instrument(gprsw=GPRswitches(root_project=root_prj), covlevel="stmt")

# Check that gnatcov does not instrument anything on reinvocation
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    out="instrument.out",
)

thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.out",
)

thistest.result()
