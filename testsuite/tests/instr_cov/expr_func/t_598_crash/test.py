"""
Regression test: GNATcoverage used to crash when Libadalang propagates a
Property_Error during expression function instrumentation. This exception was
legit (the input sources are invalid), but the crash was not.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=[])),
    covlevel="stmt",
    tolerate_messages=r"\*\*\* pkg\.ads:\d+:\d+: warning: Could not .*",
)

thistest.result()
