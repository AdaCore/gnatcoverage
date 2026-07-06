"""
Check that gnatcov warns about decision outcome exemptions when the target
decision cannot be found.
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

warnings = [
    "*** pkg.adb:8:7: warning: Could not find a decision before the end of the"
    " next statement (at 9:53) while processing exemption annotation",
    "*** pkg.adb:11:7: warning: Could not find condition #3 of SCO #8:"
    " DECISION at pkg.adb:12:20-33 while processing exemption annotation",
]

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
    trace_mode="src",
    tolerate_coverage_messages="|".join(re.escape(w) for w in warnings),
)

thistest.fail_if_not_equal(
    '"gnatcov coverage" output',
    "\n".join(warnings),
    contents_of("coverage.log").strip(),
)

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5, 6}},
        "pkg.adb.xcov": {"+": {9, 12}},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
