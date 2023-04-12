"""
Check that gnatcov correctly instruments as a main a file specified as a Main
through the Main attribute of the GPR file, but without an extension. Since
gnatcov instrument did not consider that main.adb as a main, it did not
instrument it as a main, and as a result coverage buffers were not included in
the link closure.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest

Wdir("tmp_")

gpr_obj_dir = os.path.join("..", "obj")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=os.path.join("..", "p.gpr")),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir=gpr_obj_dir,
    gpr_exe_dir=gpr_obj_dir,
    extra_coverage_args=["--annotate=xcov"],
)

check_xcov_reports("*.xcov", {"main.adb.xcov": {"+": {3}}}, gpr_obj_dir)

thistest.result()
