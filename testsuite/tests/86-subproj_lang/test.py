"""
Test that sources of another language not present in the root project are still
considered as sources of interest. This was not the case with the first version
of the parallelized instrumentation.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Sub has both Ada and C as languages, whereas Root only declares Ada sources.
# Gnatcov should nevertheless instrument C sources in Sub, as long as the
# instrumentation languages are not explicitly restricted.
prj_sub = gprfor(
    prjid="sub",
    srcdirs=["../src_sub"],
    langs=["Ada", "C"],
    mains=None,
)
prj_root = gprfor(
    prjid="root",
    srcdirs=["../src_root"],
    langs=["Ada"],
    mains=["main.adb"],
    deps=["sub.gpr"],
)

# Build and produce a coverage report for the test project. Only compute
# coverage for the C unit as this is the one that may not be instrumented.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj_root, units=["calculations.c"]),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov", {"calculations.c.xcov": {"+": {8, 9, 10, 11, 19, 20, 21, 22, 23}}}
)

thistest.result()
