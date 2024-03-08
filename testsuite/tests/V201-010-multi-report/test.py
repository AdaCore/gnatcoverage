"""
Check that gnatcov correctly produces multiple reports when requested
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

import os


tmp = Wdir("tmp_")

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])

xcov_args = build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    extra_coverage_args=["--annotate=report,xcov,dhtml", "--annotate=xml"],
    covlevel="stmt",
    mains=["main"],
    out="coverage.log",
)

# For the xcov report kind we can check the actual content
check_xcov_reports("obj/xcov", {"main.adb.xcov": {"+": {2, 5}, "-": {6}}})

# Check that there is a report in the log of the coverage command
thistest.fail_if_no_match(
    what="did not find report in the coverage output",
    regexp=r"(.|\n)*"
    r"\*\* COVERAGE REPORT \*\*"
    r"(.|\n)*"
    r"\n\*\* END OF REPORT \*\*",
    actual=contents_of("coverage.log"),
)

# For the others, we'll fall back to checking that there are files produced
# in the expected directories

thistest.fail_if(
    not os.path.exists("obj/html/main.adb.hunk.js"), "no html report produced"
)

thistest.fail_if(
    not os.path.exists("obj/xml/main.adb.xml"), "no xml report produced"
)

thistest.result()
