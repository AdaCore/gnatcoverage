"""
Regression test: "gnatcov instrument" used to crash when trying to instrument a
function whose return type is an unresolvable subtype.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(mains=[], srcdirs=[".."])),
    covlevel="stmt",
    tolerate_messages=r"\*\*\* pkg\.(adb|ads):\d+:\d+: (low_)?warning: .*",
)
thistest.result()
