"""
Regression testcase: checks that gnatcov correctly deals with mains in
dependency projects. It used to crash when there was a main in one of the
dependencies. The correct behavior should be not instrumenting them as a main.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir('tmp_')

main_prj = gprfor(srcdirs=[os.path.join("..", "src-main-dep")],
                  prjid="main_dep", mains=["main.adb"], objdir="obj-main")
root_prj = gprfor(srcdirs=[os.path.join("..", "src")], deps=[main_prj],
                  mains=["main_root.adb"], objdir="obj")

# This also checks that we do not produce multiple traces
build_run_and_coverage(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel='stmt',
    mains=["main_root"],
    extra_coverage_args=["-axcov"],
)

thistest.result()
