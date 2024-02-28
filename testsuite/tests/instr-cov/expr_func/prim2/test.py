"""
Check that we properly instrument expression functions that are primitives
of tagged types for MC/DC. The test focuses mainly on primitives where the
tagged type is the return type of the expression function. This variant of
the test does not trigger the compiler bug in T114-003
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


p_gpr = os.path.abspath("p.gpr")
obj_dir = os.path.abspath("obj")

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel="stmt+uc_mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode="src",
)

# Coverage expectations aren't really important, we want to check that
# the instrumented code is valid.

check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {6}},
        "pak.ads.xcov": {"+": {4, 5, 6, 14, 16, 17, 18, 23}, "-": {8, 10}},
        "pak.adb.xcov": {"!": {6}, "+": {8}},
    },
)

thistest.result()
