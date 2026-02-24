"""
Check that we can compute code coverage for Pure units.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

prj = gprfor(
    srcdirs=[".."],
    mains=["main.adb"],
    extra="""package Coverage is
       for Units use ("pkg", "pkg.child");
    end Coverage;
    """,
)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "pkg.ads.xcov": {"+": {12, 15}, "-": {11, 14}},
        "pkg.adb.xcov": {"+": {5, 6}, "-": {8}},
        "pkg-child.ads.xcov": {"+": {9}, "-": {10, 12, 13}},
    },
)

thistest.result()
