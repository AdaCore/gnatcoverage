"""
Ensure that a `Pragma Ada_<VERSION>` in a unit's spec does not propagate to its
body at instrumentation.
To do so, use the alternative instrumentation of elsifs, which uses if exprs
with Ada>=2012, and casts otherwise.

Note: The test is supposed to run on a Ada 2005 or below
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

_wd = Wdir("tmp_")

gpr = gprfor(
    mains=["main.adb"],
    srcdirs=["../src"],
)

result = build_and_run(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+gexpr",
    mains=["main"],
    register_failure=True,
    extra_coverage_args=[],
)

thistest.result()
