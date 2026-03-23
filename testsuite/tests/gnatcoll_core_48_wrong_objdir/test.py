"""
Regression test: check that gnatcov create instrumentation artifacts in the
right object directory. It used to create them in the wrong object directory
when the object directory value depended on the Project'Target attribute and
when the target was passed explicitly on the command line.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches

Wdir("tmp_")
for src in ("main_x86.gpr", "main_common.gpr", "main.adb"):
    cp(os.path.join("..", src), src)

build_run_and_coverage(
    gprsw=GPRswitches(root_project="main_x86.gpr"),
    mains=["main"],
    covlevel="stmt",
    gpr_exe_dir="exe",
    gpr_obj_dir="obj-x86",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    auto_config_args=False,
    extra_args=["--target=x86_64-pc-linux-gnu"],
)

check_xcov_reports("xcov", {"main.adb.xcov": {"+": {5}}})

thistest.result()
