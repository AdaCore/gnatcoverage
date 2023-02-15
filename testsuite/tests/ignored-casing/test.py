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

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov",
        "--ignore-source-files=Pkg.adb",
    ],
)

expected_reports = {
    "main.adb.xcov": {"+": {5}},
}
if env.build.os.name != "windows":
    expected_reports["pkg.adb.xcov"] = {"+": {7}}

check_xcov_reports("*.xcov", expected_reports, "obj")

thistest.result()
