"""
Test that style check issues and warnings are properly disabled in instrumented
sources.
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
    compiler_extra='for Default_Switches ("Ada") use ("-gnatwae", "-gnatyg");',
)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_gprbuild_args=["-q"],
    extra_gprbuild_cargs=["-gnatwae"],
    check_gprbuild_output=True,
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {18, 20}},
        "pkg.ads.xcov": {"+": {2}},
    },
)

thistest.result()
