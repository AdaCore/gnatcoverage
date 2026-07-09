"""
Regression test for a crash when instrumenting a library project with the
manual dump trigger. For units instrumented for manual annotation replacement
purpose, gnatcov tried to copy their non-existing SID file to the library
directory.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

# Library project holding the unit of interest ("Covered") and a unit that is
# left out of the units of interest ("Other").
mylib = gprfor(
    prjid="mylib",
    mains=[],
    srcdirs=["../lib"],
    extra="""
        for Library_Name use "mylib";
        for Library_Dir use "libdir";
    """,
)

# Root executable project, which drives the manual buffer dump.
app = gprfor(
    prjid="app",
    mains=["main.adb"],
    srcdirs=["../app"],
    deps=["mylib"],
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=app, units=["covered"]),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="app",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)

# "Other" is not a unit of interest, so it produces no report; "Covered" is
# covered normally.
check_xcov_reports(
    "xcov",
    {
        "covered.ads.xcov": {},
        "covered.adb.xcov": {"+": {4}},
    },
)

thistest.result()
