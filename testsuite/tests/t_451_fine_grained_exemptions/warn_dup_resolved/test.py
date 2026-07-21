"""
Check that gnatcov warns when two exemption annotations are identical, except
for the justification message.
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

warnings = [
    "*** main.adb:7:7: warning: Duplicate exemption at main.adb:7:7 for"
    " outcome FALSE of decision #1",
    "*** main.adb:7:7: warning: Discarding justification: never false",
    "*** main.adb:7:7: warning: In favor of: always true",
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

check_xcov_reports("xcov", {"main.adb.xcov": {"+": {9, 13}, "*": {8}}})

thistest.result()
