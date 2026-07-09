"""
Check that gnatcov warns about manual decision evaluations with unexpected
numbers of conditions.
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

warnings = [
    "*** pkg.adb:8:7: warning: SCO #6: DECISION at pkg.adb:11:10-23 has 2"
    " conditions, Manual_Decision_Evaluation annotation provides 1 values",
    "*** pkg.adb:9:7: warning: SCO #6: DECISION at pkg.adb:11:10-23 has 2"
    " conditions, Manual_Decision_Evaluation annotation provides 3 values",
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
        "main.adb.xcov": {"+": {5}},
        "pkg.adb.xcov": {"!": {11}, "-": {12}},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
