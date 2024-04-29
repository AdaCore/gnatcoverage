"""
Check the display of violations in xcov reports. They should always be
attached to the last line of the statement/decision and displayed after
its last line.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

root = gprfor(
    prjid="gen",
    srcdirs="../src",
    mains=["main.adb"],
    objdir="obj",
)

build_run_and_coverage(
    gprsw=GPRswitches(root),
    covlevel="stmt+mcdc",
    mains=["main"],
    trace_mode="src",
    extra_coverage_args=["--annotate=xcov+"],
)

# Check the contents of the report by comparing it to the exact expected report
# as we want to make sure the violations are displayed where they should.
thistest.fail_if_no_match(
    "xcov+ report",
    contents_of("../ref.xcov"),
    contents_of("obj/main.adb.xcov"),
)

thistest.result()
