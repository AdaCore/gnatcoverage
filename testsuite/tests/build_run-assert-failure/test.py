"""
Verify that the testsuite detects tests resulting in assertion
failures through build_and_run.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest, Test
from SUITE.cutils import Wdir, FatalError
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

gpr = gprfor(mains=["p.adb"], srcdirs=[".."])

# build_and_run with register_failure True and check
# the program's output for an exception indication.

try:
    build_and_run(
        gprsw=GPRswitches(root_project=gpr, units=["p"]),
        mains=["p"],
        covlevel="stmt",
        extra_coverage_args=[],
        register_failure=True
    )
except FatalError:
    pass
else:
    thistest.stop(
        FatalError(
            "expected FatalError from build_and_run, got different exception")
    )

# If we reach here, the test is PASSED. The failure registration
# closes the test logs though, so we need to reinstantiate to be able
# to report. Note that this brings us back at the test's home dir.

thistest = Test()

thistest.result()
