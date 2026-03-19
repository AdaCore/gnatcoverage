"""
Test the instrumentation (--auto-dump-buffers) of a main that is not a unit of
interest.
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
    extra="""
    package Coverage is
       for Units use ("pkg");
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
    {"pkg.adb.xcov": {"+": {5, 6, 8}}, "pkg.ads.xcov": {}},
    discard_empty=False,
)

thistest.result()
