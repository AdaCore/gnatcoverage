"""
Check that, except on Windows, the casing of glob patterns for
--ignore-source-files is significant.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

ignore_opt = "--ignore-source-files=Pkg.adb"
expected_reports = {
    "main.adb.xcov": {"+": {5}},
}
if env.build.os.name != "windows":
    expected_reports["pkg.adb.xcov"] = {"+": {7}}

gprsw = GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."]))

# First check --ignore-source-files on "gnatcov coverage"
build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov", "--output-dir=out-cov", ignore_opt
    ],
)
check_xcov_reports("out-cov", expected_reports)

# Then check it on "gnatcov instrument". This separate test makes sense as
# --ignore-source-files exercises different code paths depending on the gnatcov
# command.

build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=[ignore_opt],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"],
    trace_mode="src"
)
check_xcov_reports("out-instr", expected_reports)

thistest.result()
