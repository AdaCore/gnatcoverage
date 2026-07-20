"""
Check that "gnatcov add-annotation" correctly handles decision outcome
exemptions, and that the resulting exemptions are correctly used when computing
coverage reports.
"""

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
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
        "--location=9:7",
        "--outcome=false",
        "--justification=always true",
        "main.adb",
    ]
)
xcov(
    [
        "add-annotation",
        "--external-annotations",
        filename,
        "--output",
        filename,
        "--kind=Exempt_Decision_Outcome",
        "--location=16:7",
        "--outcome=false",
        "--decision=1",
        "--justification=never false",
        "main.adb",
    ]
)

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
)

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {10, 16, 19, 24, 26, 27, 28},
            "*": {9},
            "#": {17},
        }
    },
)

thistest.result()
