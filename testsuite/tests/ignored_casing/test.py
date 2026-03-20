"""
Check that, except on Windows, the casing of glob patterns for
--excluded-source-files is significant.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


ignore_opt = "--excluded-source-files=Pkg.adb"
expected_reports = {
    "main.adb.xcov": {"+": {5}},
    "pkg.ads.xcov": {},
}
if env.build.os.name != "windows":
    expected_reports["pkg.adb.xcov"] = {"+": {7}}

# First check --excluded-source-files on "gnatcov coverage"
thistest.log("== gnatcov coverage --excluded-source-files ==")
tmp = Wdir("tmp_cov")
gprsw = GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."]))
build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov",
        "--output-dir=out-cov",
        ignore_opt,
    ],
)
check_xcov_reports("out-cov", expected_reports)
tmp.to_homedir()

# Then check it on "gnatcov instrument". This separate test makes sense as
# --excluded-source-files exercises different code paths depending on the
# gnatcov command.
thistest.log("== gnatcov instrument --excluded-source-files ==")
tmp = Wdir("tmp_instr")
gprsw = GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."]))

build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=[ignore_opt],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"],
    trace_mode="src",
)
check_xcov_reports("out-instr", expected_reports, discard_empty=False)
tmp.to_homedir()

thistest.result()
