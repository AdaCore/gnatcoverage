"""
Check that gnatcov warns about invalid placements for Exempt_Branch
annotations.
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

warnings = [
    f"*** main.adb:{sloc}: warning: Invalid placement for a branch exemption"
    for sloc in [
        "6:4",
        "9:7",
        "11:4",
        "15:4",
        "17:7",
        "20:10",
        "22:10",
        "26:10",
        "28:4",
    ]
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
    "xcov", {"main.adb.xcov": {"+": {8, 13, 16, 19, 21}, "!": {7}, "-": {25}}}
)

thistest.result()
