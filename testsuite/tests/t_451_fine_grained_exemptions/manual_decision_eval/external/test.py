"""
Check that "gnatcov add-annotation" correctly handles manual decision
evaluations, and that the resulting annotations are correctly used when
computing coverage reports.
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
        "--kind=Manual_Decision_Evaluation",
        "--location=9:7",
        "--decision=1",
        "--justification=no worries",
        "--values=TTF",
        "main.adb",
    ]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov+",
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
            "+": {9, 10, 12, 16, 17, 20, 21, 22},
        }
    },
)

thistest.result()
