"""
Check that --annotate=xcov,report produces expected '*' lines for
exempted-and-uncovered code. It used to generate '#' lines
(exempte-and-covered) instead.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "-axcov,report",
        "--output-dir=xcov",
        "-o",
        "report.txt",
    ],
)

# Check that the xcov report contains the expected '*' and '#' lines
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5}},
        "proc.adb.xcov": {
            "+": {5, 7},
            "-": {6},
            "#": {12, 13, 14},
            "*": {8, 9, 10},
        },
    },
)

# Check that the "report" report has the right count for exempted violations
lines = [
    line
    for line in lines_of("report.txt")
    if line.startswith("proc.adb:") and "exempted violation" in line
]
thistest.fail_if_not_equal(
    'Exempted violations in the "report" report',
    "proc.adb:8:7-10:7: 1 exempted violation, justification:"
    "\nproc.adb:12:7-14:7: 0 exempted violation, justification:",
    "\n".join(lines),
)

thistest.result()
