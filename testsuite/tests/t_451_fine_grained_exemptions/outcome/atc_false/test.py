"""
Check that gnatcov warns when decision outcome True exemptions are speficied
for assertions (for which there is no obligation for their False outcome).
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

warnings = [
    "*** main.adb:8:4: warning: Discarding assertion outcome FALSE exemption"
    " (never false)",
]

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+decision+atc",
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
    "xcov", {"main.adb.xcov": {"+": {10, 20}, "!": {14}, "-": {15}}}
)

thistest.result()
