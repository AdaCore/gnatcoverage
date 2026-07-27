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
    f"*** main.c:{sloc}: warning: Invalid placement for a branch exemption"
    for sloc in [
        "11:3",
        "15:7",
        "17:3",
        "21:3",
    ]
]

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
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

check_xcov_reports("xcov", {"main.c.xcov": {"+": {9, 14, 19, 22}, "!": {12}}})

thistest.result()
