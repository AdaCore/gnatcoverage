"""
Check that we can compute code coverage for preelaborated units and reset/dump
coverage state from them.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    dump_trigger="manual",
    manual_prj_name="gen",
    trace_mode="src",
)
expected_report = {
    "main.adb.xcov": (
        {"+": {7, 8, 9, 10}}
        if thistest.options.block
        else {"+": {9, 10}, "-": {7, 8}}
    ),
    "pkg.ads.xcov": {},
    "pkg.adb.xcov": {"+": {27, 28}, "-": {30}},
}
check_xcov_reports("xcov", expected_report, discard_empty=False)

thistest.result()
