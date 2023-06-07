"""
This regression test checks that we consider the --projects / --no-subproject
switches when they are given in a project file Coverage.Switches attribute.
"""

import os

from SUITE.tutils import gprfor
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports

tmp = Wdir('tmp_')

gpr_coverage_package = """
  package Coverage is
     for Switches ("coverage") use ("--level=stmt+decision",
                                    "--projects", "b",
                                    "--no-subprojects");
   end Coverage;
"""

a_gpr = gprfor(mains=["main.adb"], prjid="a", srcdirs=os.path.join("..", "a"),
               objdir="obj-a", deps=["b"], extra=gpr_coverage_package)
b_gpr = gprfor(mains=[], prjid="b", srcdirs=os.path.join("..", "b"),
               langs=["Ada"], objdir="obj-b", deps=["c"])
c_gpr = gprfor(mains=[], prjid="c", srcdirs=os.path.join("..", "c"),
               langs=["Ada"], objdir="obj-c")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=a_gpr),
    covlevel='stmt',
    mains=["main"],
    gpr_obj_dir="obj-a",
    extra_coverage_args=['-axcov', "--output-dir=."])

# Check that we have coverage result only for the project of interest, as
# specified in the gpr file.

check_xcov_reports("*.xcov", {"b.adb.xcov": {"+": {6}}})

thistest.result()
