"""
Check that passing an explicit main that is already a unit of interest to
"gnatcov instrument"'s command line works as expected.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

gpr_file = gprfor(mains=[], srcdirs=[".."])

gprsw = GPRswitches(gpr_file)

build_run_and_coverage(gprsw=gprsw, covlevel="stmt", mains=["main"],
                       extra_coverage_args=["-axcov"],
                       extra_instr_args=["main.adb"],
                       extra_gprbuild_args=["main.adb"])

check_xcov_reports("obj", {"main.adb.xcov": {"+": {2, 5}}})

thistest.result()
