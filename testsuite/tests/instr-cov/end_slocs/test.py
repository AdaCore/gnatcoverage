"""
Regression test: gnatcov used to warn bout duplicate statement SCOs when two
statements were on the same line and not separated by a space.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


p_gpr = os.path.abspath('p.gpr')

# If it does not exist, create p.gpr's object directory to avoid noise in
# gnatcov's output.
obj_dir = os.path.abspath('obj')
if not os.path.isdir(obj_dir):
    os.mkdir(obj_dir)

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode='src')
check_xcov_reports('xcov', {'main.adb.xcov': {'+': {5}}})

thistest.result()
