"""
Test that instrumented source coverage works as expected on expression
functions.
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
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode="src",
)
check_xcov_reports("xcov", {"main.adb.xcov": {"+": {7, 9, 11, 15}, "-": {12}}})

thistest.result()
