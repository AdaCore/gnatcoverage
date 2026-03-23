"""
Test that instrumented source coverage works as expected on a setup of library
projects and one program project.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

vectors_prj = gprfor(prjid="vectors", srcdirs=["../src-vectors"], mains=[])
math_prj = gprfor(prjid="math", srcdirs=["../src-math"], mains=[])
main_prj = gprfor(
    prjid="main",
    deps=(vectors_prj, math_prj),
    srcdirs=["../src-main"],
    mains=["main.adb"],
)

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=main_prj, projects=[main_prj, math_prj, vectors_prj]
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5, 9, 11, 12, 13}},
        "vectors.ads.xcov": {"+": {6, 15, 17, 18, 19, 20}},
        "vectors.adb.xcov": {"+": {5, 10, 15, 18, 19}, "-": {16, 25, 26, 28}},
        "math.ads.xcov": {"+": {3, 11, 12, 13}},
        "math.adb.xcov": {"+": {5, 10}, "-": {15}},
    },
)

thistest.result()
