"""
Test that "gnatcov instruments" writes instrumented source files to the right
object directory when a unit (here: "pkg") has source files spread over
multiple projects (here: pkg.ads in lib.gpr and pkg.adb in root.gpr).

Both instrumented pkg.ads and pkg.adb used to be written in root.gpr's object
directory, so gprbuild was complaining that there was conflicting pkg.ads
files: one in lib.gpr's source directory, and one in root.gpr's object
directory.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

lib_gpr = gprfor(
    prjid="lib",
    srcdirs=["../src-lib"],
    mains=[],
    extra='for Excluded_Source_Files use ("pkg.adb");',
)
root_gpr = gprfor(
    prjid="root", srcdirs=["../src-root"], mains=["main.adb"], deps=["lib"]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=root_gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "lib.adb.xcov": {"+": {6}},
        "lib.ads.xcov": {},
        "main.adb.xcov": {"+": {6, 7}},
        "pkg.adb.xcov": {"+": {6}},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
