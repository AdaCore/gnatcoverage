"""
This test checks that the instrumentation of a child project produces valid
instrumented sources.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches

p_gpr = os.path.abspath('p-child.gpr')
obj_dir = os.path.abspath('obj/p-child')

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode='src')
check_xcov_reports('xcov/*.xcov',
                   {'xcov/main.adb.xcov': {'+': {5, 6}},
                    'xcov/pkg.adb.xcov': {'+': {5}}})

thistest.result()
