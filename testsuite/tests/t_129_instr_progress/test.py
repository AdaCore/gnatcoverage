"""
Check that "gnatcov instrument" correctly reports progress about the
instrumented units.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            langs=["Ada", "C", "C++"],
            mains=["main.adb"],
            srcdirs=[".."],
        ),
        units=["main", "c_unit.c", "cpp_unit.cpp"],
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    quiet=False,
    trace_mode="src",
)

# Sanity check: the instrument-build-coverage process completed with the
# expected results.
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {7, 8}},
        "c_unit.c.xcov": {"+": {7, 8}},
        "cpp_unit.cpp.xcov": {"+": {10}},
    },
)

thistest.fail_if_diff("../instrument.expected", "instrument.log")

thistest.result()
