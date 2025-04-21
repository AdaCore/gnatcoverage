"""
Check that "gnatcov instrument" works correctly when run with more than one
job.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["-j2"],
    extra_coverage_args=["--annotate=xcov"],
)
check_xcov_reports(
    "obj", {"main.adb.xcov": {"+": {7}}, "utils.c.xcov": {"+": {4}}}
)

thistest.result()
