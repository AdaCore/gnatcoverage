"""
Check that trailing Cov_Off annotations (i.e. which come last in a source file,
not paire with a Cov_On annotation) are accepted and processed as expected (D
lines until the end of the source file in coverage reports).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")
prj = gprfor(
    srcdirs=[".."],
    mains=["main.adb", "main_2.adb", "my_unit.c"],
    langs=["Ada", "C"],
)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    mains=["main", "main_2", "my_unit"],
    covlevel="stmt",
    extra_instr_args=["--external-annotations=../annotations.toml"],
    extra_coverage_args=["-axcov"],
)
check_xcov_reports(
    "obj",
    {
        "main.adb.xcov": {"+": {5}, "D": {6, 7}},
        "main_2.adb.xcov": {"+": {5, 6}, "D": {8, 9}},
        "my_unit.c.xcov": {"+": {8, 9, 10, 11, 12}},
        "my_header_1.h.xcov": {"+": {4}, "D": {5}},
        "my_header_2.h.xcov": {"+": {4}, "D": {5}},
        "my_header_3.h.xcov": {"+": {4}, "D": {5}},
    },
)

thistest.result()
