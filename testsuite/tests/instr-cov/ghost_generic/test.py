"""
Check that generics that are instantiated as ghost entites can be correctly
compiled when instrumented with --spark-compat.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--spark-compat"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {11, 15}},
        "gen.adb.xcov": {"+": {9}, "-": {4}},
        "gen.ads.xcov": {},
        "non_ghost_inst.ads.xcov": {},
    },
    discard_empty=False,
)

thistest.result()
