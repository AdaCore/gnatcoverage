"""
Check that we handle duplication and casing differences for language names in
--restricted-to-languages.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])

xcov_args = build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    extra_coverage_args=["--annotate=xcov"],
    extra_instr_args=["--restricted-to-languages=Ada,ada"],
    covlevel="stmt",
    mains=["main"],
    trace_mode="src",
)

check_xcov_reports("obj/*.xcov", {
    "obj/main.adb.xcov": {"+": {2, 5}, "-": {6}},
})

thistest.result()
