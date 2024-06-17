"""
Test that gnatcov does not crash when messages are not attached to any SCO.
It used to crash when generating a --annotate=report coverage report in
presence of such messages.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

root = gprfor(
    srcdirs="..",
    mains=["main.adb"],
    objdir="obj",
)

rep = "test.rep"

build_run_and_coverage(
    gprsw=GPRswitches(root),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=report",
        "--log=BRANCH_STATS",
        "--all-messages",
    ],
    out=rep,
)

# Check the contents of the report to ensure the expected message is indeed
# emitted (otherwise the test is pointless).
thistest.fail_if_no_match(
    "missing warning message",
    regexp=r"(.|\n)*2.3. OTHER ERRORS"
    r"(.|\n)+-----------------"
    r"(.|\n)+main\.adb:19:4: non-traceable: cond branch for"
    r" OTHER_STATEMENT"
    r"(.|\n)*",
    actual=contents_of(rep),
)

thistest.result()
