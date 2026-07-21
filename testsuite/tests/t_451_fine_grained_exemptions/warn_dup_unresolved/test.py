"""
Check that gnatcov warns about different exemption annotations that resolve to
the same fine grained exemptions but with different justification messages.
"""

import re

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

cp("../main.adb", "main.adb")
gpr = gprfor(srcdirs=["."], mains=["main.adb"])

filename = "annotations.toml"
xcov(
    [
        "add-annotation",
        "--output",
        filename,
        "--kind=Exempt_Decision_Outcome",
        "--location=6:7",
        "--outcome=false",
        "--justification=always true",
        "main.adb",
    ]
)

warnings = [
    "*** main.adb:6:7: warning: Duplicate exemption at main.adb:6:7 for"
    " outcome FALSE of decision #1",
    "*** main.adb:6:7: warning: Discarding justification: always true",
    "*** main.adb:6:7: warning: In favor of: never false",
]

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov",
        "--output-dir=xcov",
        "--external-annotations",
        filename,
    ],
    trace_mode="src",
    tolerate_coverage_messages="|".join(re.escape(w) for w in warnings),
)

thistest.fail_if_not_equal(
    '"gnatcov coverage" output',
    "\n".join(warnings),
    contents_of("coverage.log").strip(),
)

check_xcov_reports("xcov", {"main.adb.xcov": {"+": {8, 12}, "*": {7}}})

thistest.result()
